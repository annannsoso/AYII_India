
# Bring in libraries
suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(shiny))
suppressMessages(library(reactable))
suppressMessages(library(bs4Dash))
suppressMessages(library(fresh))
suppressMessages(library(DT))
suppressMessages(library(ggrepel))
suppressMessages(library(leaflet))
suppressMessages(library(sp))
suppressMessages(library(fitdistrplus))
suppressMessages(library(shinycssloaders))
source('01_Prep_Data.R', echo=FALSE)
mapping_list <- readRDS("India_Map.rds")

# Function to create plot
detrend_plot <- function(Name_Index_Par,Area_Data) {
	
	# Actual vs Detrended
	plot_dat_display <- Area_Data[NameIndex==Name_Index_Par, .(Year, Yield, Yield_Detrend)]
	plot_dat <- melt(plot_dat_display, id.vars = "Year", variable.name = "Type", value.name = "Yield")
	plot_dat[, Year := as.integer(Year)]
	plot_dat[Type=="Yield_Detrend", Type := "Yield-Detrend"]

	# Create plot
	Slope_Par <- Area_Data[NameIndex==Name_Index_Par][1, round(Slope,2)]
	R2_Par <- Area_Data[NameIndex==Name_Index_Par][1, round(R2,3)]
	Intercept_Par <- Area_Data[NameIndex==Name_Index_Par][1, round(Intercept,1)]
	Unique_Years <- plot_dat[, .N, keyby=.(Year)]$Year
	out_plot <- ggplot(plot_dat, aes(x=Year, y=Yield, col=Type)) + 
		    geom_line(lwd=3) +
	  		ggtitle("Yield by Year")+
	  		#xlab("Year")+
		    xlab(paste0("Year - y = ", Slope_Par,"x + ",Intercept_Par, "    R^{2}",'=',R2_Par))+
	  		ylab("Yield (in kg/he.)")+
		    scale_x_continuous(labels = function(x) sprintf("%.0f", x), breaks=Unique_Years)+
	  	  scale_y_continuous(labels = scales::comma)+
	  		scale_colour_manual(values=c("#0072B2","#56B4E9"))+
	  		theme_bw()
	
	# Summarize table
	plot_dat_display[, Year := as.character(Year)]
	plot_dat_display_avg <- plot_dat_display[, .(
		Year="Avg", 
		Yield=mean(Yield),
		Yield_Detrend=mean(Yield_Detrend))]
	plot_dat_display <- rbindlist(list(plot_dat_display,plot_dat_display_avg), fill=TRUE)
	
	# Formula
	out_formula <- Area_Data[NameIndex==Name_Index_Par][1, withMathJax(paste0("y=",round(Slope,2)," + ",round(Intercept,1)))]

	# Output
	out_list <- list(out_plot, plot_dat_display, out_formula)
	names(out_list) <- c("plot","table","formula")
	return(out_list)
}

# # Checks
# detrend_plot(Name_Index_Par="4-Su.Bajra-Ahmedabad-DASKROI-")
# detrend_plot(Name_Index_Par="4-WheatIrri-Ahmedabad-SANAND-DODAR")

# # Start historical burning cost
# assump_yield_detrend <- c("Detrended_Yields") #,"Untrended_Yields")
# avg_yield_calc_method <- c("7-year average (excl 2 lowest yields)") #,"5-year average (excl min and max)")
# avg_area_calc_method <- c("3-year average")
# cat_loss_tech_load <- 0.05
# info_uncertainty_load <- 0.20
# add_tech_prem <- 0.0080
# min_tech_rate <- 0.0091
# market_cycle_par <- "Soft Market"
# commerical_load_soft <- 0.10
# commerical_load_medium <- 0.30
# commerical_load_hard <- 0.50
# display_pref <- "INR"
# indem_level <- 0.80
# sum_insured <- 44000
# uptake_rate <- 0.80

# Calculate Results
calc_port <- function(
    #state = "Andhra Pradesh",
    crop_season = "Rabi",
		assump_yield_detrend="Untrended Yields", 
		avg_yield_calc_method="7-year average (excl 2 lowest yields)", 
		avg_area_calc_method="3-year average",
		cat_loss_tech_load=0.05,
		info_uncertainty_load=0.20,
		add_tech_prem=0.0080,
		min_tech_rate=0.0091,
		market_cycle_par="Soft Market",
		commerical_load_soft=0.10,
		commerical_load_medium=0.30,
		commerical_load_hard=0.50,
		crop_list_filter=c("Cumin", "Fennel", "Garlic", "Gram", "Isabgul", "Onion", "Potato", 
			"Rape&Mustard", "Su.Bajra", "Su.Groundnut", "Wheat Irri", "Wheat Unirri"),
		cluster_list_filter=as.character(1:6),
		display_pref="INR",
		Area_Data,
		Insured_Parameters,
		CropCoverageTableAct,
		dist_param=NULL) {
	
	# If name isn't in, then create one
	if (!("Coverage" %in% names(CropCoverageTableAct))) {
		CropCoverageTableAct[, Coverage := "Food grain/Oilseed Crop"]
	}
	
	# Extract market cycle parameters
	if (market_cycle_par=="Soft Market") {
		commerical_load_chosen <- commerical_load_soft
	} else if (market_cycle_par=="Medium Market") {
		commerical_load_chosen <- commerical_load_medium
	} else if (market_cycle_par=="Hard Market") {
		commerical_load_chosen <- commerical_load_hard
	} else {
		stop("Must choose Soft, Medium, Hard")	
	}
  
  
  #Introduce crop season premium factor that farm's premium contribution is based on
 crop_season_factor = 0.98
  
 if (crop_season =="Rabi"){
  crop_season_factor=0.015
 } else if (crop_season =="Kharif"){
  crop_season_factor=0.02
 } else {crop_season_factor = 100000}
 

	
	# Subset data
	Index_Data <- Area_Data[Insured_Parameters, on=.(NameIndex)]
	setnames(Index_Data, c("IndemnityLevel","SumInsuredCoverageLimit","UptakeRate"),
		c("indem_level","sum_insured","uptake_rate"))
	
	# Subset to most recent years
	#Index_Data[, .N, keyby=.(Year)]
	
	# Filter
	Index_Data <- Index_Data[Crop %in% crop_list_filter][Cluster %in% cluster_list_filter]
	
	# Are we using detrended yields or
	if (assump_yield_detrend=="Detrended Yields") {
		Index_Data[, Yield_Selected := 0.0]
		Index_Data[, Yield_Selected := as.numeric(Yield_Detrend)]
	} else {
		Index_Data[, Yield_Selected := 0.0]
		Index_Data[, Yield_Selected := as.numeric(Yield)]
	}
	
	# # Calculate mean yield given certain method 
	# setorder(Index_Data,NameIndex, -Year)
	# Index_Data[, Year_Rev_Ord := 1:.N, by=.(NameIndex)]
	# if (avg_yield_calc_method=="7-year average (excl 2 lowest yields)") {
	# 	Average_Yield_Dat <- Index_Data[Year_Rev_Ord %in% 1:7]
	# 	setorder(Average_Yield_Dat, NameIndex, Yield_Selected)
	# 	Average_Yield_Dat[, ValOrd := 1:.N, by=.(NameIndex)]
	# 	Average_Yield_Dat <- Average_Yield_Dat[!(ValOrd %in% 1:2), .(Mean_Yield_Selected=mean(Yield_Selected)), by=.(NameIndex)]	
	# } else if (avg_yield_calc_method=="5-year Average (excl. min and max)") {
	# 	Average_Yield_Dat <- Index_Data[Year_Rev_Ord %in% 1:7]
	# 	setorder(Average_Yield_Dat, NameIndex, Yield_Selected)
	# 	Average_Yield_Dat[, ValOrd := 1:.N, by=.(NameIndex)]
	# 	Average_Yield_Dat <- Average_Yield_Dat[!(ValOrd %in% c(1,.N)), .(Mean_Yield_Selected=mean(Yield_Selected)), by=.(NameIndex)]			
	# } else {
	# 	stop("Only two options")	
	# }
	# 
	
	# Calculate mean yield given certain method
	setorder(Index_Data,NameIndex, -Year)
	Index_Data[, Year_Rev_Ord := 1:.N, by=.(NameIndex)]
	if (avg_yield_calc_method=="7-year average (excl 2 lowest yields)") {
		Average_Yield_Dat <- Index_Data[Year_Rev_Ord %in% 1:7]
		setorder(Average_Yield_Dat, NameIndex, Yield)
		Average_Yield_Dat[, ValOrd := 1:.N, by=.(NameIndex)]
		Average_Yield_Dat <- Average_Yield_Dat[!(ValOrd %in% 1:2), .(Mean_Yield_Selected=mean(Yield)), by=.(NameIndex)]
	} else if (avg_yield_calc_method=="5-year Average (excl. min and max)") {
		Average_Yield_Dat <- Index_Data[Year_Rev_Ord %in% 1:7]
		setorder(Average_Yield_Dat, NameIndex, Yield)
		Average_Yield_Dat[, ValOrd := 1:.N, by=.(NameIndex)]
		Average_Yield_Dat <- Average_Yield_Dat[!(ValOrd %in% c(1,.N)), .(Mean_Yield_Selected=mean(Yield)), by=.(NameIndex)]
	} else {
		stop("Only two options")
	}
	
	# Calculate Coef of Variation
	CoefVarTable <- Index_Data[, .(CoefVarValue=sd(Yield_Selected)/mean(Yield_Selected)), keyby=.(NameIndex)]
	
	# Calculate mean area
	Average_Area_Date <- Index_Data[, .(MeanArea=mean(Area)), keyby=.(NameIndex)]	
	
	# Merge
	Average_Yield_Dat[CoefVarTable, CoefVarValue := i.CoefVarValue, on=.(NameIndex)]
	Average_Yield_Dat[Average_Area_Date, MeanArea := i.MeanArea, on=.(NameIndex)]
	Average_Yield_Dat[Index_Data, indem_level := i.indem_level, on=.(NameIndex)]
	Average_Yield_Dat[Index_Data, sum_insured := i.sum_insured, on=.(NameIndex)]
	Average_Yield_Dat[Index_Data, uptake_rate := i.uptake_rate, on=.(NameIndex)]
	
	# Build summary table all
	Summary_Table <- Average_Yield_Dat[, .(
		`Average Yield Calculation Method`=	avg_yield_calc_method,
		`Average Yield`=Mean_Yield_Selected ,
		`Indemnity Level`=indem_level,
		`Expected Notified Area Calculation Method`=avg_area_calc_method,
		`Expected Sown Area`=MeanArea,
		`Uptake Percentage`=uptake_rate,
		`Sum Insured /Coverage Limit per hectare`=sum_insured,
		`Threshold Yield`=Mean_Yield_Selected*indem_level,
		`Notified Area`=MeanArea*uptake_rate,
		`Total Sum Insured`=MeanArea*uptake_rate*sum_insured
	)]
	
	# Create year by year index table
	Index_Data[Average_Yield_Dat, Average_Yield := i.Mean_Yield_Selected, on=.(NameIndex)]
	Index_Data[Average_Yield_Dat, Average_Area := i.MeanArea, on=.(NameIndex)]
	Index_Data[Average_Yield_Dat, CoefVarValue := i.CoefVarValue, on=.(NameIndex)]
	Year_Yield_Table <- Index_Data[, .(
		NameIndex,
		Year,
		Historic_Yield=Yield_Selected,
		Threshold_Yield=Average_Yield*indem_level,
		Expected_Notified_Area=Average_Area,
		Sum_Insured_per_He=sum_insured,
		Total_Sum_Insured=Average_Area*uptake_rate*sum_insured,
		Yield_Shortfall=pmax(0,Average_Yield*indem_level-Yield_Selected)
	)]
	Year_Yield_Table[, Yield_Shortfall := pmax(0,Threshold_Yield-Historic_Yield)]
	Year_Yield_Table[, Percentage_Payout := Yield_Shortfall/Threshold_Yield]
	Year_Yield_Table[, Insurance_Payment := Total_Sum_Insured*Percentage_Payout]
	
	# Total insurance payment
	# Year_Yield_Table[, sum(Insurance_Payment)]
	
	# Distribution fitting
	if (!is.null(dist_param)) {
		
		# Calculate distribution
		Dist_Data <- copy(Year_Yield_Table)
		
		# Build pivot variable
		Dist_Data[, c("P1","P2","P3") := tstrsplit(NameIndex,"-")[c(1,2,3)]]
		Dist_Data[, Pivot := paste0(P1,"-", P2,"-",P3)]
		Dist_Data[, Pivot := P3]
		Dist_Data[, c("P1","P2","P3") := NULL]
		
		# Set seeds for reproducibility
		library(fitdistrplus)
		library(actuar)
		set.seed(1)
		
		# Aggregated dataset
		Dist_Data_Fit <- copy(Dist_Data)
		Dist_Data_Fit[, Historic_Yield_Mod := pmax(Historic_Yield,1)]
		setorder(Dist_Data_Fit, NameIndex, Year)
		
		# Fit lognornal
		if (dist_param=="lognormal") {
			FitDistObj <- Dist_Data_Fit[, {tmp=fitdist(Historic_Yield_Mod, distr="lnorm", method="mle");
				.(Est1=tmp$estimate[1], Est2=tmp$estimate[2], LogLik=tmp$loglik, AIC=tmp$aic)}, keyby=.(NameIndex)]
			Dist_Data_Fit[FitDistObj, Est1 := i.Est1, on=.(NameIndex)]
			Dist_Data_Fit[FitDistObj, Est2 := i.Est2, on=.(NameIndex)]
		} else if (dist_param=="gamma") {
			FitDistObj <- Dist_Data_Fit[, {tmp=fitdist(Historic_Yield_Mod, distr="gamma", lower=c(0,0), method="mle");
			 .(Est1=tmp$estimate[1], Est2=tmp$estimate[2], LogLik=tmp$loglik, AIC=tmp$aic)}, keyby=.(NameIndex)]
			Dist_Data_Fit[FitDistObj, Est1 := i.Est1, on=.(NameIndex)]
			Dist_Data_Fit[FitDistObj, Est2 := i.Est2, on=.(NameIndex)]			
		} else if (dist_param=="pareto") {
			Dist_Data_Fit[, StartB := min(Historic_Yield_Mod), by=.(NameIndex)]
			Dist_Data_Fit[, StartA := .N/(sum(log(Historic_Yield_Mod))-.N*(log(StartB))), by=.(NameIndex)]
			FitDistObj <- Dist_Data_Fit[, {tmp=fitdist(Historic_Yield_Mod, distr="pareto", 
				start=list(shape=StartA[1], scale=StartB[1]),lower=c(0,0), method="mle");
			 .(Est1=tmp$estimate[1], Est2=tmp$estimate[2], LogLik=tmp$loglik, AIC=tmp$aic)}, keyby=.(NameIndex)]
			Dist_Data_Fit[FitDistObj, Est1 := i.Est1, on=.(NameIndex)]
			Dist_Data_Fit[FitDistObj, Est2 := i.Est2, on=.(NameIndex)]
		} else if (dist_param=="weibull") {
			Dist_Data_Fit[, Historic_Yield_Mod := pmax(Historic_Yield,1)]
			FitDistObj <- Dist_Data_Fit[, {tmp=fitdist(Historic_Yield_Mod, distr="weibull", lower=c(0,0), method="mle");
			 .(Est1=tmp$estimate[1], Est2=tmp$estimate[2], LogLik=tmp$loglik, AIC=tmp$aic)}, keyby=.(NameIndex)]
			Dist_Data_Fit[FitDistObj, Est1 := i.Est1, on=.(NameIndex)]
			Dist_Data_Fit[FitDistObj, Est2 := i.Est2, on=.(NameIndex)]
		} else if (dist_param=="bootstrap") {
			print("Move along")
		} else {
			stop("stop fitting")	
		}

		# Bootstrap an answer
		xScenarios <- 50
		for (xIter in 1:xScenarios) {
			
			# Fit according to method
			if (dist_param=="lognormal") {
				Dist_Data_Fit[, paste0("Historic_Yield_S",xIter) := rlnorm(n=1, meanlog = Est1, sdlog = Est2), by=.(1:nrow(Dist_Data_Fit))]
			} else if (dist_param=="gamma") {
				Dist_Data_Fit[, paste0("Historic_Yield_S",xIter) := rgamma(n=1, shape = Est1, rate = Est2), by=.(1:nrow(Dist_Data_Fit))]
			} else if (dist_param=="pareto") {
				Dist_Data_Fit[, paste0("Historic_Yield_S",xIter) := rpareto(n=1, shape = Est1, rate = Est2), by=.(1:nrow(Dist_Data_Fit))]
			} else if (dist_param=="weibull") {
				Dist_Data_Fit[, paste0("Historic_Yield_S",xIter) := rweibull(n=1, shape = Est1, scale = Est2), by=.(1:nrow(Dist_Data_Fit))]
			} else if (dist_param=="bootstrap") {
				Dist_Data_Fit[, paste0("Historic_Yield_S",xIter) := sample(x=Historic_Yield, size=.N, replace = TRUE), keyby=.(NameIndex)]
			} else {
				stop("stop fitting")	
			}
			
			# Adjust losses
			Dist_Data_Fit[, paste0("Yield_Shortfall_S",xIter) := pmax(0,Threshold_Yield-get(paste0("Historic_Yield_S",xIter)))]
			Dist_Data_Fit[, paste0("Percentage_Payout_S",xIter) := get(paste0("Yield_Shortfall_S",xIter))/Threshold_Yield]
			Dist_Data_Fit[, paste0("S",xIter) := Total_Sum_Insured*get(paste0("Percentage_Payout_S",xIter))]
			Dist_Data_Fit[, (paste0("Historic_Yield_S",xIter)) := NULL]
			Dist_Data_Fit[, (paste0("Yield_Shortfall_S",xIter)) := NULL]
			Dist_Data_Fit[, (paste0("Percentage_Payout_S",xIter)) := NULL]			
		}
		
		# Key scenarios
		Scenario_Sums <- Dist_Data_Fit[, lapply(.SD, mean, na.rm=TRUE), keyby=.(NameIndex, Pivot), .SDcols=paste0("S",1:xScenarios)]
		Scenario_Sums <- Scenario_Sums[, lapply(.SD, sum, na.rm=TRUE), keyby=.(Pivot), .SDcols=paste0("S",1:xScenarios)]
		Scenario_Sums_Total <- Scenario_Sums[, lapply(.SD, sum, na.rm=TRUE), .SDcols=paste0("S",1:xScenarios)]
		
		# Clean up
		todelete <- setdiff(paste0("S",1:xScenarios), Scenario_Sums$variable)
		Dist_Data_Fit[, (todelete) := NULL]
		
		# Extract key quantiles
		Scenario_Sums_Total[, Pivot := "Total"]
		Scenario_Sums <- rbindlist(list(Scenario_Sums, Scenario_Sums_Total), fill=TRUE)
		suppressWarnings(Scenario_Sums <- melt(Scenario_Sums, id.vars = "Pivot"))
		setorder(Scenario_Sums, Pivot, value)
		Scenario_Sums[, Rank := 1:.N, by=.(Pivot)]
		key_quants <- floor(quantile(1:xScenarios, c(0.01,0.25,0.50,0.75,0.99)))
		Scenario_Sums <- Scenario_Sums[Rank %in% key_quants]
		Scenario_Sums[, QUANT := paste0("Q",c("01",25,50,75,99)), by=.(Pivot)]
		Scenario_Sums <- dcast(Scenario_Sums, Pivot~QUANT, value.var = "value")
		Expected_Sums <- Dist_Data[, .(Expected=mean(Insurance_Payment, na.rm=TRUE)), keyby=.(NameIndex, Pivot)]
		Expected_Sums <- Expected_Sums[, .(Expected=sum(Expected)), keyby=.(Pivot)]
		Expected_Sums_Total <- Expected_Sums[, .(Expected=sum(Expected))]
		Expected_Sums_Total[, Pivot := "Total"]
		Expected_Sums <- rbindlist(list(Expected_Sums, Expected_Sums_Total), fill=TRUE)
		Scenario_Sums[Expected_Sums, Expected := i.Expected, on=.(Pivot)]
		Scenario_Sums <- Scenario_Sums[, lapply(.SD, round), keyby=.(Pivot)]
		setorder(Scenario_Sums, -Expected)
		
		# What is the liklihood
		if (exists("FitDistObj")) {
			LogLikeTotal <- FitDistObj[, sum(LogLik)]
			AICTotal <- FitDistObj[, sum(AIC)]
			Scenario_Sums[, LogLike := LogLikeTotal]
			Scenario_Sums[, AIC := AICTotal]
		} else {
			Scenario_Sums[, LogLike := NA]
			Scenario_Sums[, AIC := NA]
		}
		
		
		# Return data
		return(Scenario_Sums[])

	}
	
	# Calc min/max
	Yield_Table_MeanMax <- Year_Yield_Table[, .(
		Insurance_Payment_Mean=mean(Insurance_Payment, na.rm=TRUE), 
		Insurance_Payment_Max=max(Insurance_Payment, na.rm=TRUE)
	), keyby=.(NameIndex)]
	
	# Build portfolio table
	Average_Yield_Dat[Index_Data, Crop := i.Crop, on=.(NameIndex)]
	Portfolio_Table <- Average_Yield_Dat[, .(
		Name=NameIndex,
		Crop,
		SownArea=MeanArea,
		Uptake_Level=uptake_rate,
		NotifiedArea=MeanArea*uptake_rate,
		AverageYield=Mean_Yield_Selected,
		CoefVar=CoefVarValue,
		IndemLevel=indem_level,
		ThresYield=Mean_Yield_Selected*indem_level,
		SumInsuredPerHectre=sum_insured,
		SumInsured=sum_insured*MeanArea*uptake_rate
	)]
	Portfolio_Table[Yield_Table_MeanMax, AAL := i.Insurance_Payment_Mean/(SumInsured), on=.(Name=NameIndex)]
	Portfolio_Table[Yield_Table_MeanMax, WAL := i.Insurance_Payment_Max/(SumInsured), on=.(Name=NameIndex)]
	Portfolio_Table[, Catastrophe_Loading := WAL*cat_loss_tech_load]
	Portfolio_Table[, Uncertainy_Loading := AAL*info_uncertainty_load]
	Portfolio_Table[, AreaYieldTechnicalPremium := AAL+Catastrophe_Loading+Uncertainy_Loading]
	Portfolio_Table[, TechPremDueToLocalizedAndPostHarvestPerils := (add_tech_prem*SumInsured)/SumInsured]
	Portfolio_Table[, AdditionalTechnicalPremToReachMinTechRate := NA_real_]
	Portfolio_Table[, OverallTechPremium := pmax(AreaYieldTechnicalPremium+TechPremDueToLocalizedAndPostHarvestPerils, min_tech_rate)]
	Portfolio_Table[, AdditionalTechnicalPremToReachMinTechRate := pmax(OverallTechPremium-
			AreaYieldTechnicalPremium-TechPremDueToLocalizedAndPostHarvestPerils,0)]
	Portfolio_Table[, IndicativeActuarialPremium := OverallTechPremium*(1+commerical_load_chosen)]
	Portfolio_Table[AAL %in% c(NaN,NA,Inf,-Inf), AAL := 0.0]
	Portfolio_Table[WAL %in% c(NaN,NA,Inf,-Inf), WAL := 0.0]
	Portfolio_Table[Catastrophe_Loading %in% c(NaN,NA,Inf,-Inf), Catastrophe_Loading := 0.0]
	Portfolio_Table[Uncertainy_Loading %in% c(NaN,NA,Inf,-Inf), Uncertainy_Loading := 0.0]
	Portfolio_Table[AreaYieldTechnicalPremium %in% c(NaN,NA,Inf,-Inf), AreaYieldTechnicalPremium := 0.0]
	Portfolio_Table[TechPremDueToLocalizedAndPostHarvestPerils %in% c(NaN,NA,Inf,-Inf), TechPremDueToLocalizedAndPostHarvestPerils := 0.0]
	Portfolio_Table[AdditionalTechnicalPremToReachMinTechRate %in% c(NaN,NA,Inf,-Inf), AdditionalTechnicalPremToReachMinTechRate := 0.0]	
	Portfolio_Table[OverallTechPremium %in% c(NaN,NA,Inf,-Inf), OverallTechPremium := 0.0]	
	Portfolio_Table[IndicativeActuarialPremium  %in% c(NaN,NA,Inf,-Inf), IndicativeActuarialPremium  := 0.0]	
	Portfolio_Table[CropCoverageTableAct, Coverage := i.Coverage, on=.(Crop)]
	Portfolio_Table[Coverage=="Commercial/ Horticultural", PremPaidByTheFarmers := 0.05]
	Portfolio_Table[Coverage=="Food grain/Oilseed Crop", PremPaidByTheFarmers := crop_season_factor ]
	Portfolio_Table[is.na(PremPaidByTheFarmers), PremPaidByTheFarmers := 0.015]
	Portfolio_Table[, c("Crop","Coverage") := NULL]
	Portfolio_Table[, IndicativePremSubsidies := pmax(0, IndicativeActuarialPremium-PremPaidByTheFarmers)]
	Portfolio_Table[IndicativePremSubsidies  %in% c(NaN,NA,Inf,-Inf), IndicativePremSubsidies  := 0.0]
	
	# Calculate other tables
	Total_Portfolio_BeforeAdj <- Portfolio_Table[, .(
		Total="BeforeAdj",
		SownArea=sum(SownArea),
		UptakeLevel=sum(NotifiedArea)/sum(SownArea),
		NotifiedArea=sum(NotifiedArea),
		AverageYield=sum(NotifiedArea*AverageYield)/sum(NotifiedArea),
		CoefVar=sum(NotifiedArea*CoefVar)/sum(NotifiedArea),
		IndemLevel=sum(NotifiedArea*IndemLevel)/sum(NotifiedArea),
		ThresYield=sum(NotifiedArea*ThresYield)/sum(NotifiedArea),
		SumInsuredPerHectre=sum(SumInsuredPerHectre*NotifiedArea)/sum(NotifiedArea),
		SumInsured=sum(SumInsured),
		AAL=sum(SumInsured*AAL)/sum(SumInsured),
		WAL=sum(SumInsured*WAL)/sum(SumInsured),
		Catastrophe_Loading=sum(SumInsured*Catastrophe_Loading)/sum(SumInsured),
		Uncertainy_Loading=sum(SumInsured*Uncertainy_Loading)/sum(SumInsured),
		AreaYieldTechnicalPremium=sum(SumInsured*AreaYieldTechnicalPremium)/sum(SumInsured),
		TechPremDueToLocalizedAndPostHarvestPerils=sum(SumInsured*TechPremDueToLocalizedAndPostHarvestPerils)/sum(SumInsured),
		AdditionalTechnicalPremToReachMinTechRate=sum(SumInsured*AdditionalTechnicalPremToReachMinTechRate)/sum(SumInsured),
		OverallTechPremium=sum(SumInsured*OverallTechPremium)/sum(SumInsured),
		IndicativeActuarialPremium=sum(SumInsured*IndicativeActuarialPremium)/sum(SumInsured),
		PremPaidByTheFarmers=sum(SumInsured*PremPaidByTheFarmers)/sum(SumInsured),
		IndicativePremSubsidies=sum(SumInsured*IndicativePremSubsidies)/sum(SumInsured)
	)]
	Total_Portfolio_AfterAdj <- copy(Total_Portfolio_BeforeAdj)
	Total_Portfolio_AfterAdj[, Total := "AfterAdj"]
	
	# Explore after portfolio Adjustments
	Year_Yield_Table_All <- Year_Yield_Table[, .(Insurance_Payment=sum(Insurance_Payment, na.rm=TRUE)), keyby=.(Year)]
	Total_Portfolio_AfterAdj[, AAL := Year_Yield_Table_All[, mean(Insurance_Payment)/Total_Portfolio_BeforeAdj$SumInsured[1]]]
	Total_Portfolio_AfterAdj[, WAL := Year_Yield_Table_All[, max(Insurance_Payment)/Total_Portfolio_BeforeAdj$SumInsured[1]]]
	Total_Portfolio_AfterAdj[, Catastrophe_Loading := WAL*cat_loss_tech_load]
	Total_Portfolio_AfterAdj[, Uncertainy_Loading := AAL*info_uncertainty_load]
	Total_Portfolio_AfterAdj[, AreaYieldTechnicalPremium := AAL+Catastrophe_Loading+Uncertainy_Loading]
	Total_Portfolio_AfterAdj[, TechPremDueToLocalizedAndPostHarvestPerils := add_tech_prem]
	Total_Portfolio_AfterAdj[, OverallTechPremium := pmax(AreaYieldTechnicalPremium+TechPremDueToLocalizedAndPostHarvestPerils, min_tech_rate)]
	Total_Portfolio_AfterAdj[, AdditionalTechnicalPremToReachMinTechRate := pmax(0, OverallTechPremium-AreaYieldTechnicalPremium-TechPremDueToLocalizedAndPostHarvestPerils)]
	Total_Portfolio_AfterAdj[, IndicativeActuarialPremium := OverallTechPremium*(1+commerical_load_chosen)]
	Total_Portfolio_AfterAdj[, PremPaidByTheFarmers := Portfolio_Table[, sum(SumInsured*PremPaidByTheFarmers)/sum(SumInsured)]]
	Total_Portfolio_AfterAdj[, IndicativePremSubsidies := pmax(0, IndicativeActuarialPremium-PremPaidByTheFarmers)]
	
	# Build out portfolio adjustment
	PortfolioAdj <- data.table(
		Total="Portfolio Adj",
		AAL=(Total_Portfolio_AfterAdj$AAL[1]-Total_Portfolio_BeforeAdj$AAL[1]),
		WAL=(Total_Portfolio_AfterAdj$WAL[1]-Total_Portfolio_BeforeAdj$WAL[1]),
		Catastrophe_Loading=(Total_Portfolio_AfterAdj$Catastrophe_Loading[1]-Total_Portfolio_BeforeAdj$Catastrophe_Loading[1]),
		Uncertainy_Loading=(Total_Portfolio_AfterAdj$Uncertainy_Loading[1]-Total_Portfolio_BeforeAdj$Uncertainy_Loading[1]),
		AreaYieldTechnicalPremium=(Total_Portfolio_AfterAdj$AreaYieldTechnicalPremium[1]-Total_Portfolio_BeforeAdj$AreaYieldTechnicalPremium[1]),
		TechPremDueToLocalizedAndPostHarvestPerils=(Total_Portfolio_AfterAdj$TechPremDueToLocalizedAndPostHarvestPerils[1]-
				Total_Portfolio_BeforeAdj$TechPremDueToLocalizedAndPostHarvestPerils[1]),
		OverallTechPremium=(Total_Portfolio_AfterAdj $OverallTechPremium[1]-Total_Portfolio_BeforeAdj$OverallTechPremium[1]),
		AdditionalTechnicalPremToReachMinTechRate=(Total_Portfolio_AfterAdj$AdditionalTechnicalPremToReachMinTechRate[1]-Total_Portfolio_BeforeAdj$AdditionalTechnicalPremToReachMinTechRate[1]),
		IndicativeActuarialPremium=(Total_Portfolio_AfterAdj$IndicativeActuarialPremium[1]-Total_Portfolio_BeforeAdj$IndicativeActuarialPremium[1]),
		PremPaidByTheFarmers=(Total_Portfolio_AfterAdj$PremPaidByTheFarmers[1]-Total_Portfolio_BeforeAdj$PremPaidByTheFarmers[1]),
		IndicativePremSubsidies=(Total_Portfolio_AfterAdj$IndicativePremSubsidies[1]-Total_Portfolio_BeforeAdj$IndicativePremSubsidies[1])
	)

	# Combine overall view
	Total_Portfolio_Output <- rbindlist(list(Total_Portfolio_BeforeAdj, PortfolioAdj, Total_Portfolio_AfterAdj), fill=TRUE)
	
	# Build Premium composition
	Prem_Composition <- data.table(Description=c("Insured Portfolio Average Annual Loss","Insured Portfolio Area Yield Technical Loadings",
		"Insured Portfolio Post Harvest & Localized Perils Technical Loadings",
		"Insured Portfolio Additional Technical Loadings","Insured Portfolio Technical Premium",
		"Insured Portfolio Commercial Loadings","Insured Portfolio Indicative Actuarial Premium","",
		"Estimated Premium to be paid by the Farmers","Estimated Premium Subsidies for the Insured Portfolio",
		"","Estimated Portfolio Premium Savings due to Diversification"))
	Prem_Composition[Description=="Insured Portfolio Average Annual Loss", Premium := Total_Portfolio_BeforeAdj$AAL*Total_Portfolio_BeforeAdj$SumInsured]
	Prem_Composition[Description=="Insured Portfolio Average Annual Loss", Per := Total_Portfolio_BeforeAdj$AAL]
	Prem_Composition[Description=="Insured Portfolio Area Yield Technical Loadings", Premium := Total_Portfolio_BeforeAdj$AAL*Total_Portfolio_BeforeAdj$SumInsured]
	Prem_Composition[Description=="Insured Portfolio Area Yield Technical Loadings", Per := Total_Portfolio_BeforeAdj$AAL]
	Prem_Composition[Description=="Insured Portfolio Area Yield Technical Loadings", 
		Premium := (Total_Portfolio_BeforeAdj$Catastrophe_Loading+Total_Portfolio_BeforeAdj$Uncertainy_Loading)*Total_Portfolio_BeforeAdj$SumInsured]
	Prem_Composition[Description=="Insured Portfolio Area Yield Technical Loadings", 
		Per := (Total_Portfolio_BeforeAdj$Catastrophe_Loading+Total_Portfolio_BeforeAdj$Uncertainy_Loading)]	
	Prem_Composition[Description=="Insured Portfolio Post Harvest & Localized Perils Technical Loadings", 
		Premium := (Total_Portfolio_BeforeAdj$TechPremDueToLocalizedAndPostHarvestPerils)*Total_Portfolio_BeforeAdj$SumInsured]
	Prem_Composition[Description=="Insured Portfolio Post Harvest & Localized Perils Technical Loadings", 
		Per := (Total_Portfolio_BeforeAdj$TechPremDueToLocalizedAndPostHarvestPerils)]	
	Prem_Composition[Description=="Insured Portfolio Additional Technical Loadings", 
		Premium := (Total_Portfolio_BeforeAdj$AdditionalTechnicalPremToReachMinTechRate)*Total_Portfolio_BeforeAdj$SumInsured]
	Prem_Composition[Description=="Insured Portfolio Additional Technical Loadings", 
		Per := (Total_Portfolio_BeforeAdj$AdditionalTechnicalPremToReachMinTechRate)]	
	Prem_Composition[Description=="Insured Portfolio Technical Premium", 
		Premium := (Total_Portfolio_BeforeAdj$OverallTechPremium)*Total_Portfolio_BeforeAdj$SumInsured]
	Prem_Composition[Description=="Insured Portfolio Technical Premium", 
		Per := (Total_Portfolio_BeforeAdj$OverallTechPremium)]	
	Prem_Composition[Description=="Insured Portfolio Indicative Actuarial Premium",
		Premium := (Total_Portfolio_BeforeAdj$IndicativeActuarialPremium)*Total_Portfolio_BeforeAdj$SumInsured]
	Prem_Composition[Description=="Insured Portfolio Indicative Actuarial Premium",
		Per := (Total_Portfolio_BeforeAdj$IndicativeActuarialPremium)]
	Prem_Composition[Description=="Insured Portfolio Commercial Loadings", 
		Premium := Prem_Composition[Description=="Insured Portfolio Indicative Actuarial Premium", Premium]-
			Prem_Composition[Description=="Insured Portfolio Technical Premium", Premium]]
	Prem_Composition[Description=="Insured Portfolio Commercial Loadings",
		Per := (Premium/Total_Portfolio_BeforeAdj$SumInsured)]	
	Prem_Composition[Description=="Estimated Premium to be paid by the Farmers", 
		Premium := (Total_Portfolio_BeforeAdj$PremPaidByTheFarmers)*Total_Portfolio_BeforeAdj$SumInsured]
	Prem_Composition[Description=="Estimated Premium to be paid by the Farmers", Per := Total_Portfolio_BeforeAdj$PremPaidByTheFarmers]
	Prem_Composition[Description=="Estimated Premium Subsidies for the Insured Portfolio", 
		Premium := (Total_Portfolio_BeforeAdj$IndicativePremSubsidies)*Total_Portfolio_BeforeAdj$SumInsured]
	Prem_Composition[Description=="Estimated Premium Subsidies for the Insured Portfolio", 
		Per := (Total_Portfolio_BeforeAdj$IndicativePremSubsidies)]	
	Prem_Composition[Description=="Estimated Portfolio Premium Savings due to Diversification", 
		Premium := (PortfolioAdj$IndicativePremSubsidies)*Total_Portfolio_BeforeAdj$SumInsured]
	Prem_Composition[Description=="Estimated Portfolio Premium Savings due to Diversification", 
		Per := (PortfolioAdj$IndicativePremSubsidies)]	
	
	# Create plots
	Prem_Composition_Plot_Data <- Prem_Composition[Description %in% c("Insured Portfolio Average Annual Loss",
		"Insured Portfolio Area Yield Technical Loadings","Insured Portfolio Post Harvest & Localized Perils Technical Loadings",
		"Insured Portfolio Additional Technical Loadings","Insured Portfolio Commercial Loadings")]
	Prem_Composition_Plot_Data[, Type := "Premium"]
	Prem_Composition_Chart <- ggplot(Prem_Composition_Plot_Data, aes(x=Type,y=Premium, fill=Description)) + 
		geom_bar(stat="identity", color="black")+
		ggtitle(paste0("Summary of Portfolio Commercial Premiums under the Historical Burning Cost Method (",market_cycle_par,")"))+
		xlab("")+
		ylab("Premium (INR)")+
		scale_y_continuous(labels = scales::comma)+
		scale_fill_manual(values=c("#f3c921","#6495ed","#ff8c00","#4c9a00","black"))+
		theme_bw()
	
	# Build summary of Actuarial Premium Savings
	Fiscal_Cost_Savings1 <- Prem_Composition[Description=="Estimated Premium Subsidies for the Insured Portfolio", Premium]
	Fiscal_Cost_Savings2 <- Prem_Composition[Description=="Estimated Portfolio Premium Savings due to Diversification", Premium]
	Fiscal_Cost_Savings3 <- Fiscal_Cost_Savings1+Fiscal_Cost_Savings2
	# SummaryOfFiscalCostSavings <- data.table(
	# 	 Concept=c("Premium Subsidies","Percent of TSI"),
	# 	`Premium Subsidies Before to Consider Diversification`=c(Fiscal_Cost_Savings1,Fiscal_Cost_Savings1/Total_Portfolio_BeforeAdj$SumInsured),
	# 	`Premium Subsidies After to Consider Diversification`=c(Fiscal_Cost_Savings3,Fiscal_Cost_Savings3/Total_Portfolio_BeforeAdj$SumInsured),
	# 	`Premium Subsidies Savings due to Portfolio Diversification`=c(abs(Fiscal_Cost_Savings2),abs(Fiscal_Cost_Savings2)/Total_Portfolio_BeforeAdj$SumInsured)
	# )
	SummaryOfFiscalCostSavings <- data.table(
		Type=c("Premium Subsidies Before to Consider Diversification",
				"Premium Subsidies After to Consider Diversification",
				"Premium Subsidies Savings due to Portfolio Diversification"),
		`Premium Subsidies`=c(Fiscal_Cost_Savings1, Fiscal_Cost_Savings3, Fiscal_Cost_Savings2),
		`Percent of TSI`=c(Fiscal_Cost_Savings1, Fiscal_Cost_Savings3,Fiscal_Cost_Savings2)/Total_Portfolio_BeforeAdj$SumInsured
	)
	
	# Build summary of Actuarial Premium Savings
	Actuar_Prem_Savings1 <- Prem_Composition[Description=="Insured Portfolio Indicative Actuarial Premium", Premium]
	Actuar_Prem_Savings2 <- Prem_Composition[Description=="Estimated Portfolio Premium Savings due to Diversification", Premium]
	Actuar_Prem_Savings3 <- Actuar_Prem_Savings1+Actuar_Prem_Savings2
	SummaryOfActuarCostSavings <- data.table(
		Type=c("Total Premiums Before to Consider Diversification",
				"Total Premiums After to Consider Diversification",
				"Total Premiums Savings due to Portfolio Diversification"),
		`Total Premiums`=c(Actuar_Prem_Savings1, Actuar_Prem_Savings3, Actuar_Prem_Savings2),
		`Percent of TSI`=c(Actuar_Prem_Savings1, Actuar_Prem_Savings3, Actuar_Prem_Savings2)/Total_Portfolio_BeforeAdj$SumInsured
	)

	# Make plots
	PlotOfActuarCostSavings <- ggplot(SummaryOfActuarCostSavings[1:2], aes(x=Type,y=`Total Premiums`, fill=Type)) + 
		geom_bar(stat="identity", color="black")+
		ggtitle("Summary of Actuarial Premium Savings")+
		xlab("")+
		ylab("Indicative Premium (INR)")+
		theme_bw()+
		theme(legend.position="bottom", text = element_text(size=16))+
		scale_y_continuous(labels = scales::comma)+
		scale_fill_manual(values=c("darkgreen","darkblue"))
	
	# Make second plot
	PlotOfFiscalCostSavings <- ggplot(SummaryOfFiscalCostSavings[1:2], aes(x=Type,y=`Premium Subsidies`, fill=Type)) + 
		geom_bar(stat="identity", color="black")+
		ggtitle("Summary of Fiscal Cost Savings")+
		xlab("")+
		ylab("Indicative Fiscal Cost (INR)")+
		theme_bw()+
		theme(legend.position="bottom", text = element_text(size=16))+
		scale_y_continuous(labels = scales::comma)+
		scale_fill_manual(values=c("darkgreen","darkblue"))
	
	# Make tables reactable
	SummaryOfActuarCostSavings <- reactable(SummaryOfActuarCostSavings,
			bordered=TRUE,style=list(background="#cff1d4"),
			defaultColDef=colDef(headerStyle = list(background = "#5ccb6a")),
		  
			columns = list(
				`Total Premiums` = colDef(name="Total Premiums (INR)",format = colFormat(prefix = "", separators = TRUE, digits = 0)),
				`Percent of TSI` = colDef(name="Percent of Total Sum Insured",format = colFormat(percent = TRUE,prefix = "", digits = 2))		
			)
		)
	SummaryOfFiscalCostSavings <- reactable(SummaryOfFiscalCostSavings,
		  bordered=TRUE,style=list(background="#cff1d4"),
			defaultColDef=colDef(headerStyle = list(background = "#5ccb6a")),
			columns = list(
				`Premium Subsidies` = colDef(name="Premium Subsidies (INR)",format = colFormat(prefix = "", separators = TRUE, digits = 0)),
				`Percent of TSI` = colDef(name="Percent of Total Sum Insured",format = colFormat(percent = TRUE,prefix = "", digits = 2))		
			)
		)
	

	# Return output if this is the main objective]
	out_list <- list(Total_Portfolio_Output,Portfolio_Table, Prem_Composition,Year_Yield_Table, Prem_Composition_Chart,
		PlotOfActuarCostSavings, PlotOfFiscalCostSavings,SummaryOfActuarCostSavings,SummaryOfFiscalCostSavings)
	names(out_list) <- c("Total_Portfolio_Output","Portfolio_Table","Prem_Composition","Hist_Shortfall", "Prem_Composition_Chart",
		"PlotOfActuarCostSavings","PlotOfFiscalCostSavings","SummaryOfActuarCostSavings","SummaryOfFiscalCostSavings")
	return(out_list)
}

# Calculate Results
calc_hist_burn <- function(
		NameIndexPar, 
		assump_yield_detrend="Untrended Yields", 
		avg_yield_calc_method="7-year average (excl 2 lowest yields)", 
		avg_area_calc_method="3-year average",
		cat_loss_tech_load=0.05,
		info_uncertainty_load=0.20,
		add_tech_prem=0.0080,
		min_tech_rate=0.0091,
		market_cycle_par="Soft Market",
		commerical_load_soft=0.10,
		commerical_load_medium=0.30,
		commerical_load_hard=0.50,
		display_pref="INR",
		Area_Data,
		Insured_Parameters) {
	
	# Extract market cycle parameters
	if (market_cycle_par=="Soft Market") {
		commerical_load_chosen <- commerical_load_soft
	} else if (market_cycle_par=="Medium Market") {
		commerical_load_chosen <- commerical_load_medium
	} else if (market_cycle_par=="Hard Market") {
		commerical_load_chosen <- commerical_load_hard
	} else {
		stop("Must choose Soft, Medium, Hard")	
	}
	
	# Subset data
	Index_Data <- Area_Data[NameIndex==NameIndexPar]
	setorder(Index_Data, Year)
	
	# Grab more assumptions
	indem_level <- Insured_Parameters[NameIndex==NameIndexPar, IndemnityLevel[1]]
	sum_insured <- Insured_Parameters[NameIndex==NameIndexPar, SumInsuredCoverageLimit[1]]
	uptake_rate <- Insured_Parameters[NameIndex==NameIndexPar, UptakeRate[1]]
	
	# Are we using detrended yields or
	if (assump_yield_detrend=="Detrended Yields") {
		Index_Data[, Yield_Selected := 0.0]
		Index_Data[, Yield_Selected := as.numeric(Yield_Detrend)]
	} else {
		Index_Data[, Yield_Selected := 0.0]
		Index_Data[, Yield_Selected := as.numeric(Yield)]
	}
	
	# Calculate mean yield given certain method
	setorder(Index_Data,NameIndex, -Year)
	Index_Data[, Year_Rev_Ord := 1:.N, by=.(NameIndex)]
	if (avg_yield_calc_method=="7-year average (excl 2 lowest yields)") {
		
		# For real calcs
		Average_Yield_Dat <- Index_Data[Year_Rev_Ord %in% 1:7]
		setorder(Average_Yield_Dat, NameIndex, Yield_Selected)
		Average_Yield_Dat[, ValOrd := 1:.N, by=.(NameIndex)]
		Average_Yield_Dat <- Average_Yield_Dat[!(ValOrd %in% 1:2), .(Mean_Yield_Selected=mean(Yield_Selected)), by=.(NameIndex)]	
		Average_Yield <- Average_Yield_Dat[1, Mean_Yield_Selected]
		
		# In general
		Average_Yield_Dat_General <- Index_Data[Year_Rev_Ord %in% 1:7]
		setorder(Average_Yield_Dat_General, NameIndex, Yield)
		Average_Yield_Dat_General[, ValOrd := 1:.N, by=.(NameIndex)]
		Average_Yield_Dat_General <- Average_Yield_Dat_General[!(ValOrd %in% 1:2), .(Mean_Yield_Selected=mean(Yield)), by=.(NameIndex)]	
		Average_Yield_General <- Average_Yield_Dat_General[1, Mean_Yield_Selected]
		
	} else if (avg_yield_calc_method=="5-year Average (excl. min and max)") {
		
		# For real calcs
		Average_Yield_Dat <- Index_Data[Year_Rev_Ord %in% 1:7]
		setorder(Average_Yield_Dat, NameIndex, Yield_Selected)
		Average_Yield_Dat[, ValOrd := 1:.N, by=.(NameIndex)]
		Average_Yield_Dat <- Average_Yield_Dat[!(ValOrd %in% c(1,.N)), .(Mean_Yield_Selected=mean(Yield_Selected)), by=.(NameIndex)]
		Average_Yield <- Average_Yield_Dat[1, Mean_Yield_Selected]
		
		# In general
		Average_Yield_Dat_General <- Index_Data[Year_Rev_Ord %in% 1:7]
		setorder(Average_Yield_Dat_General, NameIndex, Yield)
		Average_Yield_Dat_General[, ValOrd := 1:.N, by=.(NameIndex)]
		Average_Yield_Dat_General <- Average_Yield_Dat_General[!(ValOrd %in% c(1,.N)), .(Mean_Yield_Selected=mean(Yield)), by=.(NameIndex)]
		Average_Yield_General <- Average_Yield_Dat_General[1, Mean_Yield_Selected]
		
	} else {
		stop("Only two options")	
	}	
	
	# Calc coefficient of variation
	CoefVarValue <- Index_Data[, sd(Yield_Selected)/mean(Yield_Selected)]
	
	# Calculate mean area
	Average_Area <- Index_Data[, mean(Area)]
	
	# # Temp check
	# elig_years <- Index_Data[, .N, keyby=.(Year)][, tail(Year,9)]
	# Index_Data <- Index_Data[Year %in% elig_years]
	
	# Build summary table of assumptions
	Summary_Table <- data.table(Item=c("Expected Yield Calculation Method",
		"Expected Yield","Indemnity Level","Expected Notified Area Calculation Method",
		"Expected Sown Area","Uptake Percentage","Sum Insured /Coverage Limit per hectare",
		"Threshold Yield","Notified Area","Total Sum Insured"),
		Value=c(avg_yield_calc_method, scales::comma(Average_Yield_General), scales::percent(indem_level), 
			avg_area_calc_method, scales::comma(Average_Area), scales::percent(uptake_rate), scales::comma(sum_insured),
			scales::comma(Average_Yield*indem_level, accuracy = 0.1),scales::comma(Average_Area*uptake_rate),
			scales::comma(Average_Area*uptake_rate*sum_insured)))
	Summary_Table[, Units := ""]
	Summary_Table[Item=="Expected Yield", Units := "Kilograms per Hectare."]
	Summary_Table[Item=="Indemnity Level", Units := "% of Expected Yield"]
	Summary_Table[Item=="Expected Sown Area", Units := "Hectares."]
	Summary_Table[Item=="Uptake Percentage", Units := "% of Expected Area"]
	Summary_Table[Item=="Sum Insured /Coverage Limit per hectare", Units := "Indian Rupee"]
	Summary_Table[Item=="Threshold Yield", Units := "Kilograms per Hectare."]
	Summary_Table[Item=="Notified Area", Units := "Acres"]
	Summary_Table[Item=="Total Sum Insured", Units := "Indian Rupee"]
	
	# Insurance Payout Graph
	Insurance_Payout_Graph_Dat <- data.table(Insurance_Payout=c(0, 0, Average_Area*uptake_rate*sum_insured))
	Insurance_Payout_Graph_Dat[, Yield := c(Average_Yield*indem_level,Average_Yield*indem_level*2,0)]
	Insurance_Payout_Graph_Dat[3, Label := paste0("Maximum payout of INR\n ",
		scales::comma(Average_Area*uptake_rate*sum_insured),"\n occurs when yield is zero")]
	Insurance_Payout_Graph_Dat[2, Label := ""]
	Insurance_Payout_Graph_Dat[1, Label := paste0("Yields below ",
		scales::comma(Average_Yield*indem_level, accuracy = 0.1)," will \ntrigger non-zero \ninsurance payout.",
		"")]
	Insurance_Payout_Graph <- ggplot(Insurance_Payout_Graph_Dat, aes(x=Yield, y=Insurance_Payout, label=Label)) +
		geom_line(stat="identity", color="darkblue")+
		geom_label_repel()+
		ggtitle("Insurance Payout as a function of Crop Yield")+
		xlab("Yield (in kg/he.)")+
		ylab("Insurance Payout (in NR)")+
		scale_y_continuous(labels = scales::comma)+
		scale_x_continuous(labels = scales::comma)+
		theme_bw()		
	
	# Create year by year index table
	Year_Yield_Table <- Index_Data[, .(
		Year,
		Historic_Yield=Yield_Selected,
		Threshold_Yield=Average_Yield*indem_level,
		Expected_Notified_Area=Average_Area,
		Sum_Insured_per_He=sum_insured,
		Total_Sum_Insured=Average_Area*uptake_rate*sum_insured,
		Yield_Shortfall=pmax(0,Average_Yield*indem_level-Yield_Selected)
	)]
	Year_Yield_Table[, Percentage_Payout := Yield_Shortfall/Threshold_Yield]
	Year_Yield_Table[, Insurance_Payment := Total_Sum_Insured*Percentage_Payout]
	
	# Calculate AAL from a total persepctive
	Totals_Table <- Year_Yield_Table[, .(
		Type=c("AAL","WAL"),
		Percentage_Payout=c(mean(Percentage_Payout),max(Percentage_Payout)),
		Insurance_Payment=c(mean(Insurance_Payment),max(Insurance_Payment))
	)]
	
	# Add in extra load factors
	Extra_Load_Table <- Year_Yield_Table[, .(
		Type=c("Information Uncertainty Loading Factor (% AAL)",
			"Catastrophe Losses Loading Factor (% WAL)",
			"Additional Technical Premium due to Localized and Post-Harvest Perils (Defined by the User)"),
		Percentage_Payout=c(info_uncertainty_load,cat_loss_tech_load,add_tech_prem)
	)]
	Extra_Load_Table[Type=="Information Uncertainty Loading Factor (% AAL)", Insurance_Payment := Totals_Table[Type=="AAL",Insurance_Payment]*info_uncertainty_load]
	Extra_Load_Table[Type=="Catastrophe Losses Loading Factor (% WAL)", Insurance_Payment := Totals_Table[Type=="WAL",Insurance_Payment]*cat_loss_tech_load]
	Extra_Load_Table[Type=="Additional Technical Premium due to Localized and Post-Harvest Perils (Defined by the User)",
		Insurance_Payment := Average_Area*uptake_rate*sum_insured*add_tech_prem]
	
	# Calculate total premiums
	Total_Prem <- data.table(Type=c("Calculated Technical Premium",paste0("Additional Technical Premium to ",
			"reach the Minimum Technical Premium ,which rate is specified by the User @",
		  scales::percent(min_tech_rate,accuracy = 0.01)),
			"Selected Technical Premium (Calculated Technical Premium or Minimum Technical Premium, whichever the highest"))
	Total_Prem[1, Percentage_Payout := Totals_Table[Type=="AAL",Percentage_Payout]*(1+info_uncertainty_load) + 
			cat_loss_tech_load*Totals_Table[Type=="WAL",Percentage_Payout]+add_tech_prem]
	Total_Prem[1, Insurance_Payment := Totals_Table[Type=="AAL",Insurance_Payment]+Extra_Load_Table[, sum(Insurance_Payment)]]
	Total_Prem[3, Percentage_Payout := ifelse(Total_Prem[1, Percentage_Payout]<min_tech_rate, min_tech_rate, Total_Prem[1, Percentage_Payout])]
	Total_Prem[2, Percentage_Payout := Total_Prem[3, Percentage_Payout]-Total_Prem[1, Percentage_Payout]]
	Total_Prem[3, Insurance_Payment := Percentage_Payout*Average_Area*uptake_rate*sum_insured]
	Total_Prem[2, Insurance_Payment := Total_Prem[3, Insurance_Payment]-Total_Prem[1, Insurance_Payment]]
	
	# Actuarial Loading Factors display
	Actuarial_Loading_Factor_Display <- data.table(Type=paste0("Actuarial Premium Loading Factor -  ",c("Soft","Medium","Hard")))	
	Actuarial_Loading_Factor_Display[Type %like% "Soft", Payout := scales::percent(commerical_load_soft)]
	Actuarial_Loading_Factor_Display[Type %like% "Soft", Payment := scales::comma(commerical_load_soft*Total_Prem[3, Insurance_Payment])]
	Actuarial_Loading_Factor_Display[Type %like% "Medium", Payout := scales::percent(commerical_load_medium)]
	Actuarial_Loading_Factor_Display[Type %like% "Medium", Payment := scales::comma(commerical_load_medium*Total_Prem[3, Insurance_Payment])]
	Actuarial_Loading_Factor_Display[Type %like% "Hard", Payout := scales::percent(commerical_load_hard)]
	Actuarial_Loading_Factor_Display[Type %like% "Hard", Payment := scales::comma(commerical_load_hard*Total_Prem[3, Insurance_Payment])]
	setnames(Actuarial_Loading_Factor_Display, c("Type","Percentage Payout","Insurance Payment"))

	# Actuarial Premium display
	Actuarial_Premium_Display <- data.table(Type=paste0("Actuarial Premium -  ",c("Soft","Medium","Hard")))
	Actuarial_Premium_Display[Type %like% "Soft", Payout := scales::percent(Total_Prem[3, Percentage_Payout]*(1+commerical_load_soft), accuracy = 0.01)]
	Actuarial_Premium_Display[Type %like% "Medium", Payout := scales::percent(Total_Prem[3, Percentage_Payout]*(1+commerical_load_medium), accuracy = 0.01)]
	Actuarial_Premium_Display[Type %like% "Hard", Payout := scales::percent(Total_Prem[3, Percentage_Payout]*(1+commerical_load_hard), accuracy = 0.01)]
	Actuarial_Premium_Display[Type %like% "Soft", Payment := scales::comma(Total_Prem[3, Insurance_Payment]+commerical_load_soft*Total_Prem[3, Insurance_Payment])]
	Actuarial_Premium_Display[Type %like% "Medium", Payment := scales::comma(Total_Prem[3, Insurance_Payment]+commerical_load_medium*Total_Prem[3, Insurance_Payment])]
	Actuarial_Premium_Display[Type %like% "Hard", Payment := scales::comma(Total_Prem[3, Insurance_Payment]+commerical_load_hard*Total_Prem[3, Insurance_Payment])]
	setnames(Actuarial_Premium_Display, c("Type","Percentage Payout","Insurance Payment"))
	
	# Actuarial Loading Factors
	Actuarial_Loading_Factor <- data.table(Type=paste0("Act-Loading Factor - ",c("Soft","Medium","Hard")))
	Actuarial_Loading_Factor[1, Insurance_Payment := commerical_load_soft*Total_Prem[3, Insurance_Payment]]
	Actuarial_Loading_Factor[2, Insurance_Payment := commerical_load_medium*Total_Prem[3, Insurance_Payment]]
	Actuarial_Loading_Factor[3, Insurance_Payment := commerical_load_hard*Total_Prem[3, Insurance_Payment]]
	
	# Actuarial Premium
	Actuarial_Premiums <- data.table(Type=paste0("Act-Premium - ",c("Soft","Medium","Hard")))
	Actuarial_Premiums[1, Insurance_Payment := (1+commerical_load_soft)*Total_Prem[3, Insurance_Payment]]
	Actuarial_Premiums[2, Insurance_Payment := (1+commerical_load_medium)*Total_Prem[3, Insurance_Payment]]
	Actuarial_Premiums[3, Insurance_Payment := (1+commerical_load_hard)*Total_Prem[3, Insurance_Payment]]
	
	# Add on to table soft, medium, hard
	Year_Yield_Table[, LR_Soft := Insurance_Payment/Actuarial_Premiums[Type=="Act-Premium - Soft", Insurance_Payment]]
	Year_Yield_Table[, LR_Medium := Insurance_Payment/Actuarial_Premiums[Type=="Act-Premium - Medium", Insurance_Payment]]
	Year_Yield_Table[, LR_Hard := Insurance_Payment/Actuarial_Premiums[Type=="Act-Premium - Hard", Insurance_Payment]]
	
	# Add on to table soft, medium, hard
	Totals_Table[, LR_Soft := Insurance_Payment/Actuarial_Premiums[Type=="Act-Premium - Soft", Insurance_Payment]]
	Totals_Table[, LR_Medium := Insurance_Payment/Actuarial_Premiums[Type=="Act-Premium - Medium", Insurance_Payment]]
	Totals_Table[, LR_Hard := Insurance_Payment/Actuarial_Premiums[Type=="Act-Premium - Hard", Insurance_Payment]]
	
	# Make first graph
	Year_Yield_Table_Plot <- melt(Year_Yield_Table[, .(Year, Yield_Shortfall, Historic_Yield)], id.vars = "Year",
		variable.name = "Type", value.name = "Yield")
	Year_Yield_Table_Plot[Type=="Yield_Shortfall", Type := "Yield Shortfall"]
	Year_Yield_Table_Plot[Type=="Historic_Yield", Type := "Historic Yield"]
	Unique_Years <- Year_Yield_Table_Plot[, .N, keyby=.(Year)]$Year
	hist_yield_chart <- ggplot(Year_Yield_Table_Plot, aes(x=Year, y=Yield, fill=Type)) + 
		geom_bar(stat="identity", color="black")+
		ggtitle("Historical Yields and Shortfalls")+
		xlab("Year")+
		ylab("Yield (in kg/he.)")+
		geom_hline(yintercept=Year_Yield_Table[1, Threshold_Yield],col="red")+
		scale_y_continuous(labels = scales::comma)+
		scale_x_continuous(labels = scales::label_number(big.mark="",accuracy = 1), breaks=Unique_Years)+
		scale_fill_manual(values=c("#0072B2","#56B4E9"))+
		theme_bw()
	
	# Build plot of soft, medium, hard
	Actuarial_Premiums[, Type2 := tstrsplit(Type, " - ")[[2]]]
	Actuarial_Loading_Factor[, Type2 := tstrsplit(Type, " - ")[[2]]]
	Actuarial_Premiums[Actuarial_Loading_Factor, Loading := i.Insurance_Payment, on=.(Type2)]
	Actuarial_Premiums[, Hist_Loss := Totals_Table[Type=="AAL", Insurance_Payment]]
	Actuarial_Premiums[, Type := Type2]
	Actuarial_Premiums[, Type2 := NULL]
	Actuarial_Premiums[, Additional := Total_Prem[2, Insurance_Payment]]
	Actuarial_Premiums[, Basic := Insurance_Payment-Hist_Loss-Loading-Additional]
	Actuarial_Premiums <- Actuarial_Premiums[, .(Type, Hist_Loss, Basic, Additional,Loading,Insurance_Payment)]
	Actuarial_Premiums[Type %like% "Soft", Type := "Soft Market"]
	Actuarial_Premiums[Type %like% "Medium", Type := "Medium Market"]
	Actuarial_Premiums[Type %like% "Hard", Type := "Hard Market"]
	Actuarial_Premiums_Plot <- melt(Actuarial_Premiums[, .(Scenario=Type, Hist_Loss, Basic, Loading)], id.vars = "Scenario",
		variable.name = "Type", value.name = "Amount")
	Actuarial_Premiums_Plot[, Scenario := factor(Scenario, levels=c("Soft Market","Medium Market","Hard Market"))]
	Actuarial_Premiums_Plot[, Type := factor(Type, levels=c("Loading","Basic","Hist_Loss"), labels = c("Commercial Premium Loading","Basic","Histortical Annual Average Loss"))]
	scenario_chart <- ggplot(Actuarial_Premiums_Plot, aes(x=Scenario, y=Amount, fill=Type)) + 
		geom_bar(stat="identity", color="black")+
		ggtitle("Scenarios")+
		xlab("Scenario")+
		ylab("Commerical Premium")+
		scale_y_continuous(labels = scales::comma)+
		scale_fill_manual(values=c("#0072B2","#56B4E9","#FF5733"))+
		theme_bw()+
		theme(legend.position="bottom")
	
	# Relabel Totals_Table
	Totals_Table[Type=="AAL", Type := "Average Annual Losses"]
	Totals_Table[Type=="WAL", Type := "Worst Annual Losses"]
	
	# Return everything
	out_list <- list(Summary_Table,Year_Yield_Table,Totals_Table,Extra_Load_Table,
		Total_Prem,Actuarial_Loading_Factor,Actuarial_Premiums,
		Year_Yield_Table_Plot,hist_yield_chart,
		Actuarial_Premiums_Plot,scenario_chart,Insurance_Payout_Graph,Actuarial_Loading_Factor_Display,Actuarial_Premium_Display)
	names(out_list) <- c("Summary_Table","Year_Yield_Table","Totals_Table",
		"Extra_Load_Table","Total_Prem","Actuarial_Loading_Factor","Actuarial_Premiums",
		"Year_Yield_Table_Plot","hist_yield_chart","Actuarial_Premiums_Plot",
		"scenario_chart","Insurance_Payout_Graph","Actuarial_Loading_Factor_Display","Actuarial_Premium_Display")
	return(out_list)
}


# Create ui
ui = bs4DashPage(
	
	# Page setup
	title = paste0("AYII"),
	sidebar_collapsed = TRUE,
	enable_preloader = FALSE,
	controlbar_overlay = FALSE,
	navbar = bs4DashNavbar(
		status = "white",
		rightUi = NULL
	),
	
	# Setup sidebard
	sidebar = bs4DashSidebar(
		
		# Sidebar options
		expand_on_hover = TRUE,
		skin = "light",
		status = "primary",
		title = "AYII",
		brandColor = "primary",
		url = NULL,
		src = NULL,
		elevation = 3,
		opacity = 0.8,
		
		# Side bar menu
		bs4SidebarMenu(
			id = "current_tab",
			flat = FALSE,
			compact = FALSE,
			child_indent = TRUE,
			
			# Header of sidebar
			bs4SidebarHeader("Assumptions"),
			
			bs4SidebarMenuItem(
				"Assumptions Input",
				tabName = "assump_tab",
				icon = "globe-americas"
			),
			
			bs4SidebarMenuItem(
				"Slope",
				tabName = "slope_tab",
				icon = "globe-americas"
			),
			
			bs4SidebarMenuItem(
				"Historical Burning",
				tabName = "burn_tab",
				icon = "globe-americas"
			),
			
			bs4SidebarMenuItem(
				"Portfolio",
				tabName = "port_tab",
				icon = "globe-americas"
			)		
		)
	),
	
	
	body = bs4DashBody(
		
	    use_theme(create_theme(
        bs4dash_color(
          blue = "#acd984",
          navy = "#01DF3A"
        )
      )),
			
		############# Assumptions ####################
		bs4TabItems(
			
		bs4TabItem(
			 tabName = "assump_tab",
			
			bs4TabCard(
				id="Assumptions_Input_Tab",
				title = "Assumptions", 
				closable = FALSE,
				maximizable = TRUE,
				width = 12,
				status = "gray",
				solidHeader = FALSE,
				collapsible = TRUE,
				
				bs4TabPanel(
					tabName="Data Input",
					
					 # Another tab card
					 bs4TabCard(
						id="Data_Input_TabPanel",
						title = "Upload", 
						closable = FALSE,
						maximizable = TRUE,
						width = 12,
						status = "gray",
						solidHeader = FALSE,
						collapsible = TRUE,
					 	
					 bs4TabPanel(
						tabName="Upload Files",
					
					  # Yield file
					  fileInput("yield_file", "Choose Yield File",
        			accept = c(
          			"text/csv",
          			"text/comma-separated-values,text/plain",
          			".csv")
      			),
					  fileInput("area_file", "Choose Area File",
        			accept = c(
          			"text/csv",
          			"text/comma-separated-values,text/plain",
          			".csv")
      			),
					  fileInput("insured_file", "Choose Insured Parameters File",
        			accept = c(
          			"text/csv",
          			"text/comma-separated-values,text/plain",
          			".csv")
					  ),	
        		
					 	# Show any errors or duplicates or non-overlap
					 	uiOutput("duplicate_ui_par"),
					 	uiOutput("overlap_ui_par")
					 	
					 	),
					 	
					 bs4TabPanel(
						tabName="Area-Yield Table",
					 	reactableOutput("yield_area_raw")
					 ),
					 bs4TabPanel(
						tabName="Insured Parameters",
					 	reactableOutput("insured_param_reactable")
					 )			 	
					 	
					 )
					
				),
				
				# Subcards
				bs4TabPanel(
					tabName="Model Parameters",
					
						fluidRow(
						column(width=6,
						# tags$p("Coverage Estimation Parameters",
					  #		selectInput("trend_detrend_assump","Underlying Yield Detrending Assumption", c("Untrended Yields","Detrended Yields")),
					  #		selectInput("avg_yield_calc_method","Average Yield Calculation Method", c("7-year average (excl 2 lowest yields)",
					  #			"5-year Average (excl. min and max)")),
					  #		selectInput("area_calc_method","Estimated Notified Area Calculation Method", c("3 Year Average","Long Term Average",
					  #			"5 Year Average (excl. min and max)"))
					  #	),
					  	
		# 			  	# Technical Premium Loading Factors
		# 			  	tags$p("Technical Loading Parameters",
		# 			  		numericInput("cat_loss_tech_load_wal","Catastrophe Losses Technical Load (% WAL)", value=0.05,min=0.0, max=1.0),
		# 			  		numericInput("info_uncertain_load_aal","Information Uncertainy Load (% AAL)", value=0.20,min=0.0, max=1.0),
		# 			  		numericInput("exceed_prob_pml","Exceedance Probability For PML", value=0.01,min=0.0, max=1.0),
		# 			  		numericInput("info_uncertain_load","Information Uncertainy Load", value=0.20,min=0.0, max=1.0),
		# 			  	),
		# 			  	
		# 			  	# Additional Technical Premium
		# 			  	tags$p("Additional Technical Premium due to Localized and Post Harvest Losses",
		# 			  		numericInput("add_tech_prem","Additional Technical Premium due to Localized and Post Harvest Losses", value=0.0080,min=0.0, max=1.0)
		# 			  	),
		# 			  	
		# 			  	# Minimum Technical Rate
		# 			  	tags$p("Minimum Technical Rate",
		# 			  		numericInput("min_tech_rate","Minimum Technical Rate", value=0.0091,min=0.0, max=1.0)
		# 			  	),
		#   				
		#   				# Market variables
		# 			  	tags$p("Market Type",
		# 			  		selectInput("market_par","Market Type", c("Soft Market","Medium Market","Hard Market"), "Soft Market")
		# 			  	),  
						
		                 
		        
            tags$p("PMFBY Participant State",
		          selectInput("State", "PMFBY Participant State", c("Gujarat", "Karnataka", "Maharastra", 
		                                                            "Odisha","Uttar Pradesh"))),
                 
		        tags$p("Crop Season Type",
		          selectInput("CropSeason", "Crop Season", c("Rabi", "Kharif"))),
		        
		
				  	tags$p("Coverage Estimation Parameters",
				  		selectInput("trend_detrend_assump","Underlying Yield Detrending Assumption", c("Untrended Yields","Detrended Yields")),
				  		selectInput("avg_yield_calc_method","Average Yield Calculation Method", c("7-year average (excl 2 lowest yields)",
				  			"5-year Average (excl. min and max)","Enter Manually")),
				  		selectInput("area_calc_method","Estimated Notified Area Calculation Method", c("3 Year Average","Long Term Average",
				  			"5 Year Average (excl. min and max)"))
				  	),
		
		# Technical Premium Loading Factors
		tags$p("Technical Loading Parameters",
		       numericInput("cat_loss_tech_load_wal","Catastrophe Losses Technical Load (% WAL)", value=0.05,min=0.0, max=1.0),
		       numericInput("info_uncertain_load_aal","Information Uncertainy Load (% AAL)", value=0.20,min=0.0, max=1.0),
		       numericInput("exceed_prob_pml","Exceedance Probability For PML", value=0.01,min=0.0, max=1.0),
		       numericInput("info_uncertain_load","Information Uncertainy Load", value=0.20,min=0.0, max=1.0),
		
		       )
		),
				  	
						column(width=6,
					  	
					  	
					  	# Additional Technical Premium
					  	tags$p("Additional Technical Premium due to Localized and Post Harvest Losses",
					  		numericInput("add_tech_prem","Additional Technical Premium due to Localized and Post Harvest Losses", value=0.0080,min=0.0, max=1.0)
					  	),
					  	
					  	# Minimum Technical Rate
					  	tags$p("Minimum Technical Rate",
					  		numericInput("min_tech_rate","Minimum Technical Rate", value=0.0091,min=0.0, max=1.0)
					  	),
		  				
		  				# Market variables
					  	tags$p("Market Type",
					  		selectInput("market_par","Market Type", c("Soft Market","Medium Market","Hard Market"), "Soft Market")
					  	),  	
			  				
							# Market variables
						  	tags$p("Commerical Load",
						  		numericInput("comm_load_soft_par","Soft", value=0.10,min=0.0, max=1.0),
						  		numericInput("comm_load_medium_par","Medium", value=0.30,min=0.0, max=1.0),
						  		numericInput("comm_load_hard_par","Hard", value=0.50,min=0.0, max=1.0)
						  	)
							
						))
				),
				bs4TabPanel(
					tabName="Crop Class",
				  # Crop coverage table
				  uiOutput("crop_select_ui"),
					tableOutput("crop_cov_assump")
				)
			)
		),
		bs4TabItem(
			 tabName = "slope_tab",
	
  		# Location
			uiOutput("location1_ui_par"),
			
			bs4TabCard(
				id="Slope_Mini_Tab",
				title = "Slopes", 
				closable = FALSE,
				maximizable = TRUE,
				width = 12,
				status = "gray",
				solidHeader = FALSE,
				collapsible = TRUE,
				
				# Subcards
				bs4TabPanel(
					tabName="Plot",
					#uiOutput("slope_text"),
					plotOutput("slope_plot")
				),
				bs4TabPanel(
					tabName="Table",
					reactableOutput("slope_table")
				)
			)				
  	),
		
		bs4TabItem(
			 tabName = "burn_tab",
			
			uiOutput("location2_ui_par"),
			
			bs4TabCard(
				id="Burn_Mini_Tabs",
				title = "Burns", 
				closable = FALSE,
				maximizable = TRUE,
				width = 12,
				status = "gray",
				solidHeader = FALSE,
				collapsible = TRUE,
				
				# Subcards
				bs4TabPanel(
					tabName="Scenarios",
					plotOutput("scenario_plot"),
					reactableOutput("act_prem"),
				),
				bs4TabPanel(
					tabName="Assumptions",
					reactableOutput("assump_table"),
					plotOutput("insurance_payout_plot")
				),
				bs4TabPanel(
					tabName="Year View",
					plotOutput("year_yield_plot"),
					tags$br(),
	  			reactableOutput("year_yield_table"),
					tags$br(),
	  			reactableOutput("aal_wal"),
					tags$br(),
	  			reactableOutput("extra_load"),
					tags$br(),
	  			reactableOutput("total_prem"),
					tags$br(),
	  			reactableOutput("act_load_factor_display"),
					tags$br(),
	  			reactableOutput("act_prem_display"),
				)
			)		
  	),
		
		bs4TabItem(
			 tabName = "port_tab",
			
			uiOutput("cropcluster_par_ui"),
			
			bs4TabCard(
				id="Port_Mini_Tabs",
				title = "Ports", 
				closable = FALSE,
				maximizable = TRUE,
				width = 12,
				status = "gray",
				solidHeader = FALSE,
				collapsible = TRUE,
				
				# Subcards
				bs4TabPanel(
					tabName="Actuarial Premium Savings",
	  			plotOutput("actuar_plot"),
					tags$br(),
					reactableOutput("actuar_table")
				),
				bs4TabPanel(
					tabName="Fiscal Cost Savings",
	  			plotOutput("fiscal_plot"),
					tags$br(),
	  			reactableOutput("fiscal_table")
				),
				bs4TabPanel(
					tabName="Premium Composition",
	  			reactableOutput("port_prem_composition"),
					tags$br(),
					plotOutput("port_prem_composition_plot")
				),
				bs4TabPanel(
					tabName="Detailed Breakdown",
					reactableOutput("port_react_total"),
					tags$br(),
  				reactableOutput("port_react_raw")
				),
				bs4TabPanel(
					tabName="Map",
					leafletOutput("leaflet_map_out")
				)
			# Remove all PML calculation and distribution fitting
			#	bs4TabPanel(
			#		tabName="PML",
			#		tags$br(),
			#		selectInput("pml_dist", "Distribution Assumptions", choices = c("bootstrap","lognormal","gamma","weibull"), selected = "bootstrap"),
			#		tags$p("This function calculates 50 scenarios"),
			#		actionButton("fitPML_button", "Calculate Scenarios"),
			#		tags$br(),
			#		tags$br(),
		  #	  shinycssloaders::withSpinner(reactableOutput("pml_table")),
		  #	  tags$br(),
			#		shinycssloaders::withSpinner(tableOutput("loglike_table"))
			# )
			)	
  	)

		
	)),
	footer = bs4DashFooter(
		copyrights = a(
			href = "https://www.worldbank.org/en/about", 
			target = "_blank", "@World Bank"
		),
		right_text = "2020"
	)
)

# Server
server = function(input, output, session) {
	
	# Read in datasets
	dynamic_ds_react <- reactive({
		
		# Grab files
		# req(input$yield_file)
		# req(input$area_file)
		# req(input$insured_file)
		
		# Read datasets and extract
	  Out_Data <- read_input_files(Area_File=input$area_file$datapath, Yield_File=input$yield_file$datapath, Insured_File=input$insured_file$datapath)
	  return(Out_Data)
	})
	
	
	# Create dynamic ui
	output$crop_select_ui <- renderUI({
		req(dynamic_ds_react())
		Crop_UI_List <- lapply(dynamic_ds_react()$Crop_Input$Crop, function(x) 
			selectInput(inputId = paste0("crop_ui_",x), label=x, choices = c("Food grain/Oilseed Crop","Commercial/ Horticultural"), "Food grain/Oilseed Crop"))
		Crop_UI_List <- shiny::tagList(Crop_UI_List)
		Crop_UI_List
	})
	
	# Create reactive UI
	CropCoverageTable <- reactive({
		req(dynamic_ds_react())
		Crop_Assumps <- copy(dynamic_ds_react()$Crop_Input)
		# print(input$crop_ui_Cumin)
		# print(input[[paste0("crop_ui_Cumin")]])
		Crop_Assumps[, Coverage := input[[paste0("crop_ui_",Crop)]], by=.(1:nrow(Crop_Assumps))]
		return(Crop_Assumps[])
	})
	
	# Deal with duplicates
	output$duplicate_ui_par <- renderUI({
		req(dynamic_ds_react())
		dynamic_ds_react()$Duplicate_List
	})
	
	# Deal with lack of overlap
	output$overlap_ui_par <- renderUI({
		req(dynamic_ds_react())
		dynamic_ds_react()$Overlap_Txt
	})
	
	# Name list
	output$location1_ui_par <- renderUI({
		req(dynamic_ds_react())
		selectInput('location_par1', 'Location', choices=dynamic_ds_react()$Named_Items, selected="4-Su.Bajra-Ahmedabad-DASKROI-1")	
	})
	output$location2_ui_par <- renderUI({
		req(dynamic_ds_react())
		selectInput('location_par2', 'Location', choices=dynamic_ds_react()$Named_Items, selected="4-Su.Bajra-Ahmedabad-DASKROI-")
	})
	
	# Build reactive function
	react_slope_list <- reactive({
		# Create plot with trend
		req(input$location_par1)
		req(dynamic_ds_react())
		out_list <- detrend_plot(Name_Index_Par=input$location_par1, Area_Data=dynamic_ds_react()$Area_Data)
		return(out_list)
	})
	
	# Render UI for cluster or crop selection
	output$cropcluster_par_ui <- renderUI({
		# Requirements
		req(dynamic_ds_react())
		crop_list <- dynamic_ds_react()$Area_Data[, .N, keyby=.(Crop)]$Crop
		cluster_list <- dynamic_ds_react()$Area_Data[, .N, keyby=.(Cluster)]$Cluster
		fluidRow(
			selectInput("crop_select_par", "Crop Selection", crop_list, crop_list, multiple=TRUE),
			selectInput("cluster_select_par", "Cluster Selection", cluster_list, cluster_list, multiple=TRUE)			
		)
	})
	
	# Output Insured Parameters
	output$insured_param_reactable <- renderReactable({
		req(dynamic_ds_react())
		reactable(dynamic_ds_react()$Insured_Parameters,
			bordered=TRUE,style=list(background="#cff1d4",fontSize = "12px"),
			defaultColDef=colDef(headerStyle = list(background = "#5ccb6a")),
			columns = list(
				IndemnityLevel = colDef(name="Indemnity Level",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 1)),
				SumInsuredCoverageLimit = colDef(name="Sum Insured Coverage Limit",format = colFormat(prefix = "", separators = TRUE, digits = 2)),
				UptakeRate = colDef(name="Uptake Rate",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 1))				
			)
		)
	})
	
	# Calculate portfolio
	port_react <- reactive({
		
		# Requirements
		req(input$crop_select_par)
		req(input$cluster_select_par)
		req(input$trend_detrend_assump)
		req(input$avg_yield_calc_method)
		req(input$area_calc_method)
		req(input$cat_loss_tech_load_wal)
		req(input$info_uncertain_load_aal)
		req(input$add_tech_prem)
		req(input$min_tech_rate)
		req(input$market_par)
		req(input$comm_load_soft_par)
		req(input$comm_load_medium_par)
		req(input$comm_load_hard_par)
		req(dynamic_ds_react())
		req(CropCoverageTable())
		req(input$State)
	
	
		# Feed portfolio function
		portfolio_output <- calc_port(
		
		  #state_input = input$State,
		  crop_season = input$CropSeason,
			assump_yield_detrend=input$trend_detrend_assump, 
			avg_yield_calc_method=input$avg_yield_calc_method, 
			avg_area_calc_method=input$area_calc_method,
			cat_loss_tech_load=input$cat_loss_tech_load_wal,
			info_uncertainty_load=input$info_uncertain_load_aal,
			add_tech_prem=input$add_tech_prem,
			min_tech_rate=input$min_tech_rate,
			market_cycle_par=input$market_par,
			commerical_load_soft=input$comm_load_soft_par,
			commerical_load_medium=input$comm_load_medium_par,
			commerical_load_hard=input$comm_load_hard_par,
			crop_list_filter=input$crop_select_par,
			cluster_list_filter=input$cluster_select_par,
			display_pref="INR",
			Area_Data=dynamic_ds_react()$Area_Data,
			Insured_Parameters=dynamic_ds_react()$Insured_Parameters,
			CropCoverageTableAct=copy(CropCoverageTable()))
		
		# Return data
		return(portfolio_output[])
			
	})
	
	# Remove all PML calculation and distribution fitting	
	# PML reactive
#	pml_estimates <- eventReactive(input$fitPML_button, {
#		
#		# Requirements
#		req(input$crop_select_par)
#		req(input$cluster_select_par)
#		req(input$trend_detrend_assump)
#		req(input$avg_yield_calc_method)
#		req(input$area_calc_method)
#		req(input$cat_loss_tech_load_wal)
#		req(input$info_uncertain_load_aal)
#		req(input$add_tech_prem)
#		req(input$min_tech_rate)
#		req(input$market_par)
#		req(input$comm_load_soft_par)
#		req(input$comm_load_medium_par)
#		req(input$comm_load_hard_par)
#		req(dynamic_ds_react())
#		req(CropCoverageTable())
#		req(input$pml_dist)
#		
#		# Feed portfolio function
#		pml_output <- calc_port(
#			assump_yield_detrend=input$trend_detrend_assump, 
#			avg_yield_calc_method=input$avg_yield_calc_method, 
#			avg_area_calc_method=input$area_calc_method,
#			cat_loss_tech_load=input$cat_loss_tech_load_wal,
#			info_uncertainty_load=input$info_uncertain_load_aal,
#			add_tech_prem=input$add_tech_prem,
#			min_tech_rate=input$min_tech_rate,
#			market_cycle_par=input$market_par,
#			commerical_load_soft=input$comm_load_soft_par,
#			commerical_load_medium=input$comm_load_medium_par,
#			commerical_load_hard=input$comm_load_hard_par,
#			crop_list_filter=input$crop_select_par,
#			cluster_list_filter=input$cluster_select_par,
#			display_pref="INR",
#			Area_Data=dynamic_ds_react()$Area_Data,
#			Insured_Parameters=dynamic_ds_react()$Insured_Parameters,
#			CropCoverageTableAct=copy(CropCoverageTable()),
#			dist_param=input$pml_dist)
#			
#		# Loglike table
#		LogLikeTable <- pml_output[1, .(Dist=input$pml_dist, LogLike, AIC)]
#		pml_output[, c("LogLike","AIC") := NULL]
		
#		# Outout
#		out_list <- list(LogLikeTable, pml_output)
#		names(out_list) <- c("LogLikeTable","pml_output")
#		return(out_list)
#				
#	})
	
	# # What is the output?
	# output$pml_table <- renderTable({
	# 	pml_estimates()$pml_output
	# })
	
	# Render reactable output for portfolio each
	#output$pml_table <- renderReactable({
		
		# Required elemtns
	#	req(pml_estimates())
		
		# Format reactable table
		#print(names(port_react()$Portfolio_Table))
#		PML_React <- reactable(pml_estimates()$pml_output,filterable = FALSE,
#			bordered=TRUE,style=list(background="#cff1d4"),
#			defaultColDef=colDef(headerStyle = list(background = "#5ccb6a")),
#		  columns = list(
#		  	Pivot = colDef(name="District",style=list(background="#ebe7e7")),
#		    Q01 = colDef(name="Q01",format = colFormat(prefix = "", separators = TRUE, digits = 0)),
#		  	Q25 = colDef(name="Q25",format = colFormat(prefix = "", separators = TRUE, digits = 0)),
#		  	Q50 = colDef(name="Q50",format = colFormat(prefix = "", separators = TRUE, digits = 0)),
#		  	Q75 = colDef(name="Q75",format = colFormat(prefix = "", separators = TRUE, digits = 0)),
#		  	Q99 = colDef(name="Q99",format = colFormat(prefix = "", separators = TRUE, digits = 0)),
#		  	Expected = colDef(name="Expected",format = colFormat(prefix = "", separators = TRUE, digits = 0))
#		  ))		
#		PML_React
#	})
	
	# What is the output?
	output$loglike_table <- renderTable({
		pml_estimates()$LogLikeTable
	})
	
	# Fiscal Cost plot
	output$fiscal_plot <- renderPlot({
		req(port_react())
		port_react()$PlotOfFiscalCostSavings
	})
	
	# Actuarial Premium savings plot
	output$actuar_plot <- renderPlot({
		req(port_react())
		port_react()$PlotOfActuarCostSavings
	})
	
	# Fiscal Cost table
	output$fiscal_table <- renderReactable({
		req(port_react())
		port_react()$SummaryOfFiscalCostSavings
	})
	
	# Actuarial Premium  table
	output$actuar_table <- renderReactable({
		req(port_react())
		port_react()$SummaryOfActuarCostSavings
	})
	
	
	# Render reactable output for portfolio each
	output$port_react_raw <- renderReactable({
		
		# Required elemtns
		req(port_react())
		
	  # Define Market Condition
	  MarketCondition=paste( "Indicative Actuarial Premium under ",  input$market_par )
	  MarketCondition=paste( MarketCondition, " (INR) (18)")
	  
		# Format reactable table
		# print(names(port_react()$Portfolio_Table))
		Portfolio_React <- reactable(port_react()$Portfolio_Table,filterable = TRUE,
			bordered=TRUE,style=list(background="#cff1d4"),
			defaultColDef=colDef(headerStyle = list(background = "#5ccb6a",fontSize="14px"), style=list(fontSize="14px")),
		  columns = list(
		  	Name = colDef(style=list(background="#ebe7e7")),
		    SownArea = colDef(name="Sown Area (hes) (1)",format = colFormat(prefix = "", separators = TRUE, digits = 0)),
		  	Uptake_Level = colDef(name="Uptake Level (2)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 1)),
		  	NotifiedArea = colDef(name="Notified Area (hes) (3)",format = colFormat(prefix = "", separators = TRUE, digits = 1)),
		  	AverageYield = colDef(name="Expected Yield (kg/he) (4)",format = colFormat(prefix = "", separators = TRUE, digits = 1)),
		  	CoefVar = colDef(name="Coef Var (5)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 1)),
		  	IndemLevel = colDef(name="Indem Level (6)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 1)),
		  	ThresYield = colDef(name="Thres Yield (kg/he) (7)",format = colFormat(prefix = "", separators = TRUE, digits = 1)),
		  	SumInsuredPerHectre = colDef(name="Sum Insured Per Hectre (INR) (8)",format = colFormat(prefix = "", separators = TRUE, digits = 0)),
		  	SumInsured = colDef(name="Sum Insured (INR) (9)",format = colFormat(prefix = "", separators = TRUE, digits = 0)),
		  	AAL = colDef(name="Average Annual Losses (AAL) (INR) (10)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
		  	WAL = colDef(name="Worst Annual Losses (WAL) (INR) (11)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
		  	Catastrophe_Loading  = colDef(name="Catastrophe Loading (INR) (12)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
		  	Uncertainy_Loading  = colDef(name="Uncertainy Loading (INR) (13)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
		  	AreaYieldTechnicalPremium  = colDef(name="Area Yield Technical Premium (INR) (10)+(12)+(13) (14)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
		  	TechPremDueToLocalizedAndPostHarvestPerils  = colDef(name="Tech Prem Due To Localized And Post Harvest Perils (INR) (15)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
		  	AdditionalTechnicalPremToReachMinTechRate  = colDef(name="Additional Technical Prem To Reach MinTechRate (INR) (16)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
		  	OverallTechPremium  = colDef(name="Overall Tech Premium (INR) (15)+(16) (17)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
		  	IndicativeActuarialPremium  = colDef(name=MarketCondition, format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
		  	PremPaidByTheFarmers  = colDef(name="Prem Paid By The Farmers (INR) (19)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
		  	IndicativePremSubsidies  = colDef(name="Indicative Prem Subsidies (INR) (20)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2))
		  ))		
		Portfolio_React
	})
	
	# Render reactable output for portfolio each
	output$port_react_total <- renderReactable({
		reactable(port_react()$Total_Portfolio_Output)
	  
	  # Define Market Condition
	  MarketCondition=paste( "Indicative Actuarial Premium under ",  input$market_par )
	  MarketCondition=paste( MarketCondition, " (INR) (18)")
	  
		# Format reactable table
		Total_Portfolio_React <- reactable(port_react()$Total_Portfolio_Output,
			bordered=TRUE,style=list(background="white"),
			defaultColDef=colDef(headerStyle = list(background = "#5ccb6a",fontSize="14px"), style=list(fontSize="14px")),
		  columns = list(
		    SownArea = colDef(name="Sown Area (hes) (1)",format = colFormat(prefix = "", separators = TRUE, digits = 0)),
		  	UptakeLevel = colDef(name="Uptake Level (2)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 1)),
		  	NotifiedArea = colDef(name="Notified Area (hes) (3)",format = colFormat(prefix = "", separators = TRUE, digits = 1)),
		  	AverageYield = colDef(name="Expected Yield (kg/he) (4)",format = colFormat(prefix = "", separators = TRUE, digits = 1)),
		  	CoefVar = colDef(name="Coef Var (5)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 1)),
		  	IndemLevel = colDef(name="Indem Level (6)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 1)),
		  	ThresYield = colDef(name="Thres Yield (kg/he) (7)",format = colFormat(prefix = "", separators = TRUE, digits = 1)),
		  	SumInsuredPerHectre = colDef(name="Sum Insured Per Hectre (INR) (8)",format = colFormat(prefix = "", separators = TRUE, digits = 0)),
		  	SumInsured = colDef(name="Sum Insured (INR) (9)",format = colFormat(prefix = "", separators = TRUE, digits = 0)),
		  	AAL = colDef(name="Average Annual Losses (AAL) (INR) (10)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
		  	WAL = colDef(name="Worst Annual Losses (WAL) (INR) (11)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
		  	Catastrophe_Loading  = colDef(name="Catastrophe Loading (INR) (12)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
		  	Uncertainy_Loading  = colDef(name="Uncertainy Loading (INR) (13)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
		  	AreaYieldTechnicalPremium  = colDef(name="Area Yield Technical Premium (INR) (10)+(12)+(13) (14)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
		  	TechPremDueToLocalizedAndPostHarvestPerils  = colDef(name="Tech Prem Due To Localized And Post Harvest Perils (INR) (15)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
		  	AdditionalTechnicalPremToReachMinTechRate  = colDef(name="Additional Technical Prem To Reach MinTechRate (INR) (16)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
		  	OverallTechPremium  = colDef(name="Overall Tech Premium (INR) (15)+(16) (17)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
		  	IndicativeActuarialPremium  = colDef(name=MarketCondition ,format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
		  	PremPaidByTheFarmers  = colDef(name="Prem Paid By The Farmers (INR) (19)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
		  	IndicativePremSubsidies  = colDef(name="Indicative Prem Subsidies (INR) (20)",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2))
		  )		
		)
		Total_Portfolio_React
	})
	
	# Render reactable output for portfolio each
	output$port_prem_composition <- renderReactable({
		# Require data
		req(port_react())
		
		# Format reactable table
		Prem_Composition_React <- reactable(port_react()$Prem_Composition,defaultPageSize=12,
				bordered=TRUE,style=list(background="#cff1d4"),
				defaultColDef=colDef(headerStyle = list(background = "#5ccb6a")),
			  columns = list(
			    Premium = colDef(name="Premium (INR)",format = colFormat(prefix = "", separators = TRUE, digits = 0)),
			  	Per = colDef(name="% Total Sum Insured",format = colFormat(percent = TRUE, digits = 2))
			  )		
			)
			Prem_Composition_React			
	})
	
	# Render reactable output for portfolio each
	output$port_prem_composition_plot <- renderPlot({
		# Require data
		req(port_react())
		port_react()$Prem_Composition_Chart
	})
	
	# Output area and yield assumptions by year
	output$yield_area_raw <- renderReactable({
		req(dynamic_ds_react())
		react_yield_raw <- dynamic_ds_react()$Area_Data[, .(NameIndex, Cluster, Crop, District, Taluka, GramPanchayat,NotificationLevel,Year,Area,Yield)]
		year_options <- react_yield_raw[, .N, keyby=.(Year)]$Year
		year_col_defs <- lapply(year_options, function(x) colDef(format = colFormat(separators = TRUE,digits = 1)))
		names(year_col_defs) <- as.character(year_options)
		react_yield_raw1 <- copy(react_yield_raw)
		react_yield_raw1[, Type := "Area"]
		react_yield_raw1 <- dcast(react_yield_raw1, NameIndex+Cluster+Crop+District+Taluka+GramPanchayat+NotificationLevel+Type~Year, value.var="Area")
		react_yield_raw2 <- copy(react_yield_raw)
		react_yield_raw2[, Type := "Yield"]
		react_yield_raw2 <- dcast(react_yield_raw2, NameIndex+Cluster+Crop+District+Taluka+GramPanchayat+NotificationLevel+Type~Year, value.var="Yield")
		react_yield_raw <- rbindlist(list(react_yield_raw2,react_yield_raw1), fill=TRUE)
		rm(react_yield_raw1, react_yield_raw2)
		react_yield_raw[, NameIndex := NULL]
		react_yield_raw <- reactable(react_yield_raw,
			bordered=TRUE,style=list(background="#cff1d4",fontSize = "12px"),
			defaultColDef=colDef(headerStyle = list(background = "#5ccb6a")),
			columns = c(list(
			    GramPanchayat = colDef(name="Gram Panchayat"),
					NotificationLevel = colDef(name="Notification Level")),
				  (year_col_defs)
			 ), filterable = TRUE)	
		react_yield_raw
	})
	
	# Present reactive elements from above
	output$slope_plot <- renderPlot({
		
		# Requirements
		req(input$location_par1)
		req(react_slope_list())
		
		# Plot
		out_plot <- react_slope_list()$plot
		out_plot
	})
	
	# Print out table
	output$slope_table <- renderReactable({
		
		# Requirements
		req(input$location_par1)
		req(react_slope_list())
		
		# Plot
		out_table <- react_slope_list()$table
		reactable(out_table, bordered=TRUE,style=list(background="#cff1d4"),
			defaultColDef=colDef(headerStyle = list(background = "#5ccb6a")),
			columns = list(
				Year=colDef(style=list(background="#ebe7e7")),
				`Yield` = colDef(name="Yield (kg/he)",format = colFormat(prefix = "", separators = TRUE, digits = 0)),
				`Yield_Detrend` = colDef(name="Yield Detrend (kg/he)",format = colFormat(prefix = "", separators = TRUE, digits = 0))
			), defaultPageSize=15
		)
	})
	
	# Print out text
	output$slope_text <- renderUI({
		
		# Requirements
		req(input$location_par1)
		req(react_slope_list())
		
		# Plot
		out_table <- react_slope_list()$formula
		out_table		
	})
	
	# Build reactive function
	react_calc_results <- reactive({

		# Requirements
		req(input$location_par2)
	  #req(input$State)
	  #req(input$CropSeason)
		req(input$trend_detrend_assump)
		req(input$avg_yield_calc_method)
		req(input$area_calc_method)
		req(input$cat_loss_tech_load_wal)
		req(input$info_uncertain_load_aal)
		req(input$add_tech_prem)
		req(input$min_tech_rate)
		req(input$market_par)
		req(input$comm_load_soft_par)
		req(input$comm_load_medium_par)
		req(input$comm_load_hard_par)	
		req(dynamic_ds_react())
		
		# Calculate historical burn
		out_list <- calc_hist_burn(
			NameIndexPar=input$location_par2, 
			#state_input = input$State,
			#crop_season = input$CropSeason,
			assump_yield_detrend=input$trend_detrend_assump, 
			avg_yield_calc_method=input$avg_yield_calc_method, 
			avg_area_calc_method=input$area_calc_method,
			cat_loss_tech_load=input$cat_loss_tech_load_wal,
			info_uncertainty_load=input$info_uncertain_load_aal,
			add_tech_prem=input$add_tech_prem,
			min_tech_rate=input$min_tech_rate,
			market_cycle_par=input$market_par,
			commerical_load_soft=input$comm_load_soft_par,
			commerical_load_medium=input$comm_load_medium_par,
			commerical_load_hard=input$comm_load_hard_par,
			display_pref="INR",
			Area_Data=dynamic_ds_react()$Area_Data,
			Insured_Parameters=dynamic_ds_react()$Insured_Parameters) 
		
		return(out_list)
	})
	
	# Assumptions table
	output$assump_table <- renderReactable({
		# Requirements
		req(input$location_par2)
		req(react_calc_results())
		Summary_Assump_Table <- copy(react_calc_results()$Summary_Table)
		reactable(Summary_Assump_Table,bordered=TRUE,style=list(background="#cff1d4"),
			defaultColDef=colDef(headerStyle = list(background = "#5ccb6a")),
			columns = list(
				Item=colDef(style=list(background="#ebe7e7"))
			)
		)
	})
	
	# Year by year table
	output$year_yield_table <- renderReactable({
		# Requirements
		req(input$location_par2)
		req(react_calc_results())
		setorder(react_calc_results()$Year_Yield_Table, Year)
		reactable(react_calc_results()$Year_Yield_Table, bordered=TRUE,style=list(background="#cff1d4", fontSize = "12px"),
			defaultColDef=colDef(headerStyle = list(background = "#5ccb6a")),
			columns = list(
				Year=colDef(style=list(background="#ebe7e7")),
				`Historic_Yield` = colDef(name="Historic Yield (kgr/he)",format = colFormat(prefix = "", separators = TRUE, digits = 0)),
				`Threshold_Yield` = colDef(name="Threshold Yield (kgr/he)",format = colFormat(prefix = "", separators = TRUE, digits = 1)),
				`Expected_Notified_Area` = colDef(name="Expected Notified Area (hectares)",format = colFormat(prefix = "", separators = TRUE, digits = 0)),
				`Sum_Insured_per_He` = colDef(name="Sum Insured per He",format = colFormat(prefix = "", separators = TRUE, digits = 0)),
				`Total_Sum_Insured` = colDef(name="Total Sum Insured (INR)",format = colFormat(prefix = "", separators = TRUE, digits = 0)),
				`Yield_Shortfall` = colDef(name="Yield Shortfall",format = colFormat(prefix = "", separators = TRUE, digits = 1)),
				`Percentage_Payout` = colDef(name="Percentage Payout",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
				`Insurance_Payment` = colDef(name="Insurance Payment",format = colFormat(prefix = "", separators = TRUE, digits = 1)),
				`LR_Soft` = colDef(name="LR Soft",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
				`LR_Medium` = colDef(name="LR Medium",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
				`LR_Hard` = colDef(name="LR Hard",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2))
			)			
		)
	})	
	
	# AAL and WALK
	output$aal_wal <- renderReactable({
		# Requirements
		req(input$location_par2)
		req(react_calc_results())
		reactable(react_calc_results()$Totals_Table, bordered=TRUE,style=list(background="#cff1d4"),
			defaultColDef=colDef(headerStyle = list(background = "#5ccb6a")),
			columns = list(
				Type=colDef(style=list(background="#ebe7e7")),
				`Percentage_Payout` = colDef(name="Percentage Payout",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
				`Insurance_Payment` = colDef(name="Insurance Payment",format = colFormat(prefix = "", separators = TRUE, digits = 1)),
				`LR_Soft` = colDef(name="LR Soft",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
				`LR_Medium` = colDef(name="LR Medium",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
				`LR_Hard` = colDef(name="LR Hard",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2))
			)	 			
		)
	})		
	
	# Extra load
	output$extra_load <- renderReactable({
		# Requirements
		req(input$location_par2)
		req(react_calc_results())
		reactable(react_calc_results()$Extra_Load_Table, bordered=TRUE,style=list(background="#cff1d4"),
			defaultColDef=colDef(headerStyle = list(background = "#5ccb6a")),
	  	columns = list(
	  		Type=colDef(style=list(background="#ebe7e7")),
	  		`Percentage_Payout` = colDef(name="Percentage Payout",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
	  		`Insurance_Payment` = colDef(name="Insurance Payment",format = colFormat(prefix = "", separators = TRUE, digits = 1))
			)	  				
		)
	})	
	
	# Actuarial loading table
	output$act_load_factor_display <- renderReactable({
		# Requirements
		req(input$location_par2)
		req(react_calc_results())
		reactable(react_calc_results()$Actuarial_Loading_Factor_Display, bordered=TRUE,style=list(background="#cff1d4"),
			defaultColDef=colDef(headerStyle = list(background = "#5ccb6a")),
	  	columns = list(
	  		Type=colDef(style=list(background="#ebe7e7")),
	  		`Percentage Payout` = colDef(name="Percentage Payout",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
	  		`Insurance Payment` = colDef(name="Insurance Payment",format = colFormat(prefix = "", separators = TRUE, digits = 1))
			)	  				
		)
	})
	
	# Actuarial loading table
	output$act_prem_display <- renderReactable({
		# Requirements
		req(input$location_par2)
		req(react_calc_results())
		reactable(react_calc_results()$Actuarial_Premium_Display, bordered=TRUE,style=list(background="#cff1d4"),
			defaultColDef=colDef(headerStyle = list(background = "#5ccb6a")),
	  	columns = list(
	  		Type=colDef(style=list(background="#ebe7e7")),
	  		`Percentage Payout` = colDef(name="Percentage Payout",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
	  		`Insurance Payment` = colDef(name="Insurance Payment",format = colFormat(prefix = "", separators = TRUE, digits = 1))
			)	  				
		)
	})


	# Total premium
	output$total_prem <- renderReactable({
		# Requirements
		req(input$location_par2)
		req(react_calc_results())
		reactable(react_calc_results()$Total_Prem, bordered=TRUE,style=list(background="#cff1d4"),
			defaultColDef=colDef(headerStyle = list(background = "#5ccb6a")),
			columns = list(
				Type=colDef(style=list(background="#ebe7e7")),
				`Percentage_Payout` = colDef(name="Percentage Payout",format = colFormat(percent=TRUE,prefix = "", separators = TRUE, digits = 2)),
				`Insurance_Payment` = colDef(name="Insurance Payment",format = colFormat(prefix = "", separators = TRUE, digits = 1))
			)	  			
		)
	})	
	
	# Total premium
	output$act_prem <- renderReactable({
		# Requirements
		req(input$location_par2)
		req(react_calc_results())
		#print(names(react_calc_results()$Actuarial_Premiums))
		reactable(react_calc_results()$Actuarial_Premiums,bordered=TRUE,style=list(background="#cff1d4"),
			defaultColDef=colDef(headerStyle = list(background = "#5ccb6a")),
			columns = list(
				Type=colDef(name="Market Type", style=list(background="#ebe7e7")),
				Hist_Loss=colDef(name="Historical Annual Average Loss",format = colFormat(prefix = "", separators = TRUE, digits = 0)),
				Basic=colDef(name="Basic Technical Loading",format = colFormat(prefix = "", separators = TRUE, digits = 0)),
				Additional=colDef(name="Additional Technical Loading",format = colFormat(prefix = "", separators = TRUE, digits = 0)),
				Loading=colDef(name="Commercial Premium Loading",format = colFormat(prefix = "", separators = TRUE, digits = 0)),
				Insurance_Payment=colDef(name="Commercial Premium",format = colFormat(prefix = "", separators = TRUE, digits = 0))
			)
		)
	})	
	
	# Plot yield
	output$year_yield_plot <- renderPlot({
		# Requirements
		req(input$location_par2)
		req(react_calc_results())	
		react_calc_results()$hist_yield_chart	
	})
	
	# Plot burning
	output$insurance_payout_plot <- renderPlot({
		# Requirements
		req(input$location_par2)
		req(react_calc_results())	
		react_calc_results()$Insurance_Payout_Graph
	})

	# Plot scenarios
	output$scenario_plot <- renderPlot({
		# Requirements
		req(input$location_par2)
		req(react_calc_results())	
		react_calc_results()$scenario_chart	
	})
	
	# Geo plot
	leaflet_react <- reactive({
		
		# Requirement
		req(port_react())
		
		# Merge and summarize
		Portfolio_Table <- copy(port_react()$Portfolio_Table)
		Portfolio_Table[, District := tstrsplit(Name,"-")[3]]
		Portfolio_Table[mapping_list$district_mapping, District_Mod := i.District_Mod, on=.(District=DistrictRaw)]
		Mapping_Summary <- Portfolio_Table[, .(
			AAL=weighted.mean(x=AAL, w=SumInsured),
			WAL=weighted.mean(x=WAL, w=SumInsured),
			AverageYield=weighted.mean(x=AverageYield, w=NotifiedArea),
			NotifiedArea=sum(NotifiedArea)
		), keyby=.(District=District_Mod)]
		
		# Map data
		Map_Data <- mapping_list$ind_map@data
		Map_Data[Mapping_Summary, Value := i.AAL, on=.(NAME_C=District)]
		Map_Data[, Label := paste0(NAME_C, " - ", scales::comma(Value))]

		# Mapping
		mapping_list$ind_map@data <- copy(Map_Data)
		qpal <- colorQuantile("RdYlBu", mapping_list$ind_map$Value, n = 5)
		leaflet_outplot <- leaflet(mapping_list$ind_map) %>%
			addTiles() %>%
		  addPolygons(
		    fillColor=~qpal(Value),
		    weight = 2,
		    opacity = 1,
		    color = "white",
		    dashArray = "3",
		    fillOpacity = 0.7,
		    highlight = highlightOptions(
		      weight = 5,
		      color = "#666",
		      dashArray = "",
		      fillOpacity = 0.7,
		      bringToFront = TRUE),
		    label = mapping_list$ind_map$Label,
		    labelOptions = labelOptions(
		      style = list("font-weight" = "normal", padding = "3px 8px"),
		      textsize = "15px",
		      direction = "auto")) %>%
		   addLegend(pal = qpal, values = ~Value, opacity = 1)
		
		# Return map
		return(leaflet_outplot)
		
	})
	
	# Render leaflet
	output$leaflet_map_out <- renderLeaflet({
		req(leaflet_react())
		leaflet_react()
	})
	
}

# Run app
app <- shinyApp(ui=ui, server=server)
app

