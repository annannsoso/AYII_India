

# Read in function
read_input_files <- function(Area_File, Yield_File, Insured_File) {
	
	# Look if files exist
	if (is.null(Area_File)) {
		Area_File <- "Area_Data.csv"
	}
	if (is.null(Yield_File)) {
		Yield_File <- "Yield_Data.csv"
	}
	if (is.null(Insured_File)) {
		Insured_File <- "Insured_Parameters.csv"
	}
	
	# Bring in csvs
	Area_Data <- fread(Area_File, encoding="UTF-8")
	Yield_Data <- fread(Yield_File, encoding="UTF-8")
	Insured_Parameters <- fread(Insured_File, encoding="UTF-8")
	
	# Change name
	if ("Name Index" %in% names(Insured_Parameters)) {
		setnames(Insured_Parameters, "Name Index", "NameIndex")
	}
	
	# Build crop list
	Crop_Dat <- Yield_Data[, .N, keyby=.(Crop)][, N := NULL][]

	# Remove spaces
	Area_Data[, `Name Index` := gsub("\\s+","", `Name Index`)]
	Yield_Data[, `Name Index` := gsub("\\s+","", `Name Index`)]
	Insured_Parameters[, `NameIndex` := gsub("\\s+","", `NameIndex`)]
	
	# Check overlap from 3 files
	Overlap_Check <- data.table(NameIndex=c(unique(Area_Data$`Name Index`), unique(Yield_Data$`Name Index`), unique(Insured_Parameters$NameIndex)))
	Overlap_Check <- Overlap_Check[, .N, keyby=.(NameIndex)][N != 3]
	Overlap_Check <- Overlap_Check[, .(NameIndex)]
	if (nrow(Overlap_Check)==0) {
		Overlap_Txt <- paste0("Area, Yield, and Insured Parameters have 100% overlap")	
	} else {
		Overlap_Txt <- paste0("Area, Yield, and Insured Parameters do not have 100% overlap. Here are some examples ",
			"of name indexs without full coverage ", paste0(head(Overlap_Check$NameIndex,10), collapse = ", "))
	}
	Overlap_Txt <- tags$ul(Overlap_Txt)
	
	# Melt area data
	Area_Data <- melt(Area_Data, id.vars = c("Number Index", "Name Index", "Cluster", "Crop", "District", 
		"Taluka", "Gram Panchayat", "Notification Level"), variable.name = "Year", value.name = "Area")
	Area_Data[, Area := as.numeric(gsub(",","",Area))]
	Area_Data[is.na(Area), Area := 0.0]
	
	# Look for duplicates
	Area_Data[, Cnt := .N, keyby=.(Cluster, Crop, District, Taluka, `Gram Panchayat`, Year)]
	Area_Duplicate_Cnt <- Area_Data[Cnt>1, .N]
	Area_Duplicate_Examples <- tail(Area_Data[Cnt>1, unique(`Name Index`)],10)
	if (Area_Duplicate_Cnt==0) {
		Area_Duplicate_Text <- paste0("Area Data has zero duplicate records")
	} else {
		Area_Duplicate_Text <- paste0("Area Data has ",Area_Duplicate_Cnt," duplicate records and ",
			"some examples include:",paste0(Area_Duplicate_Examples, collapse = ", "))	
	}
	Area_Data[, Cnt := NULL]
	Area_Data <- unique(Area_Data)

	# Melt yield data
	Yield_Data <- melt(Yield_Data, id.vars = c("Number Index", "Name Index", "Cluster", "Crop", "District", 
		"Taluka", "Gram Panchayat", "Notification Level"), variable.name = "Year", value.name = "Yield")
	Yield_Data[, Yield := as.numeric(gsub(",","",Yield))]
	Yield_Data[is.na(Yield), Yield := 0.0]
	
	# Look for duplicates
	Yield_Data[, Cnt := .N, keyby=.(Cluster, Crop, District, Taluka, `Gram Panchayat`, Year)]
	Yield_Duplicate_Cnt <- Yield_Data[Cnt>1, .N]
	Yield_Duplicate_Examples <- tail(Yield_Data[Cnt>1, unique(`Name Index`)],10)
	if (Yield_Duplicate_Cnt==0) {
		Yield_Duplicate_Text <- paste0("Yield Data has zero duplicate records")
	} else {
	  Yield_Duplicate_Text <- paste0("Yield Data has ",Yield_Duplicate_Cnt," duplicate records and ",
			"some examples include:",paste0(Yield_Duplicate_Examples, collapse = ", "))	
	}
	Yield_Data[, Cnt := NULL]
	Yield_Data <- unique(Yield_Data)

	# Merge data
	Area_Data[Yield_Data, Yield := i.Yield, on=.(`Number Index`,Year)]
	Area_Data[, `Number Index` := NULL]
	Area_Data <- unique(Area_Data)
	
	# Should be unique at this level
	Area_Data[, .N, keyby=.(Crop, District, Taluka, `Gram Panchayat`,Year)][N>1]
	Area_Data[, Index := .GRP, by=.(Crop, District, Taluka, `Gram Panchayat`)]
	
	# Clean up names
	new_names <- gsub("\\s+","",names(Area_Data))
	setnames(Area_Data, names(Area_Data), new_names)
	Area_Data[, Year := as.numeric(as.character(Year))]
	
	# Read in Insurance paramtes
	Insured_Parameters[, IndemnityLevel := gsub("%","", IndemnityLevel)]
	Insured_Parameters[, IndemnityLevel := as.numeric(IndemnityLevel)/100]
	Insured_Parameters[, SumInsuredCoverageLimit := gsub(",","", SumInsuredCoverageLimit)]
	Insured_Parameters[, SumInsuredCoverageLimit := as.numeric(SumInsuredCoverageLimit)]
	Insured_Parameters[, UptakeRate := gsub("%","", UptakeRate)]
	Insured_Parameters[, UptakeRate := as.numeric(UptakeRate)/100]
	Insured_Parameters[is.na(SumInsuredCoverageLimit), SumInsuredCoverageLimit := 0.0]
	Insured_Parameters[, NameIndex := gsub("\\s+","",NameIndex)]
	Area_Data[, NameIndex := gsub("\\s+","",NameIndex)]
	Area_Data[Insured_Parameters, Ind := 1.0, on=.(NameIndex)]
	Area_Data[, Ind := NULL]
	
	# Bring in data
	setorder(Area_Data, Index, Year)
	Area_Data[, Area := as.numeric(Area)]
	Area_Data[, Yield := as.numeric(Yield)]
	
	# List of locations
	name_index_list <- Area_Data[, .N, keyby=.(NameIndex)]$NameIndex
	
	# Read in Insurance paramtes
	Insured_Parameters[, IndemnityLevel := as.numeric(IndemnityLevel)]
	Insured_Parameters[, SumInsuredCoverageLimit := as.numeric(SumInsuredCoverageLimit)]
	Insured_Parameters[, UptakeRate := as.numeric(UptakeRate)]
	
	# Take unique Insured Parameters
	Insured_Parameters[, Cnt := .N, keyby=.(NameIndex)]
	Insured_Parameters_Duplicate_Cnt <- Insured_Parameters[Cnt>1, .N]
	Insured_Parameters_Examples <- tail(Insured_Parameters[Cnt>1, unique(NameIndex)],10)
	if (Insured_Parameters_Duplicate_Cnt==0) {
		Insured_Parameters_Duplicate_Text <- paste0("Insured Parameters Data has zero duplicate records")
	} else {
	  Insured_Parameters_Duplicate_Text <- paste0("Insured Parameters Data has ",Insured_Parameters_Duplicate_Cnt," duplicate records and ",
			"some examples include:",paste0(Insured_Parameters_Examples, collapse = ", "))	
	}
	Insured_Parameters[, Cnt := NULL]
	Insured_Parameters <- unique(Insured_Parameters)
	
	# Combine text of duplicated
	Duplicate_List <- tags$ul(
		tags$li(Area_Duplicate_Text),
		tags$li(Yield_Duplicate_Text),
		tags$li(Insured_Parameters_Duplicate_Text)
	)

	# Calculate trended yields
	Slope_Calcs <- Area_Data[, 
		{
	      ux <- mean(Year)
	      uy <- mean(Yield)
	      slope <- sum((Year - ux) * (Yield - uy)) / sum((Year - ux) ^ 2)
	      inter <- uy - slope * ux
	      r2 <- 1-(sum((Yield - (slope * Year)-inter)^2))/(sum(((Yield-uy)^2)))
	      list(MinYear=min(Year), Slope=slope, Intercept=inter, R2=r2)
	}, keyby=.(Index)]
	Slope_Calcs[, Intercept := ((MinYear-1)*Slope+Intercept)]
	Slope_Calcs[, R2 := round(R2,3)]
	
	# Detrend yields
	Area_Data[Slope_Calcs, Slope := i.Slope, on=.(Index)]
	Area_Data[Slope_Calcs, Intercept := i.Intercept, on=.(Index)]
	Area_Data[Slope_Calcs, R2 := i.R2, on=.(Index)]
	Area_Data[, Yield_Detrend := Yield+Slope*(max(Year)-Year), by=.(Index)]
	#Area_Data[Slope<0, Yield_Detrend := Yield] # not sure if this makes sense
	
	# Return big list
	big_list <- list(name_index_list, Area_Data, Insured_Parameters,Overlap_Txt,Duplicate_List,Crop_Dat)
	names(big_list) <- c("Named_Items","Area_Data","Insured_Parameters","Overlap_Txt","Duplicate_List","Crop_Input")
	return(big_list)
	
}

