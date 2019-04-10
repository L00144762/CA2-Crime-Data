#1 (a) - Importing NIPostcode data and showing 
# total rows, structure and first 10 rows
# header = FASLE; when header= TRUE it complicates the table  and is counterintuitive
# stringasfactors = FALSE; don't want r to treat strings as factors - might cause probelems

NI_Postcode <- read.csv('NIPostcodes.csv', header = FALSE, sep = ",", 
                     strip.white = TRUE, stringsAsFactors = FALSE)
                     
str(NI_Postcode)
nrow(NI_Postcode)
head(NI_Postcode, 10)


# (b) colum names

colnames(NI_Postcode) <- c("Organisation Name","Sub-Building Name", "Building Name", "Number",
                        "Primary Thorfare","Alt Thorfare", "Secondary Thorfare", "Locality", 
                        "Townland", "Town", "County","Postcode", "x-coordinates",
                        "y-coordinates", "Primary Key (identifier)")



# pasting colums together; tidying up dataframe
NI_Pcode <- NI_Postcode
NI_Pcode$`Secondary Thorfare` <- paste(NI_Pcode$`Alt Thorfare`, NI_Pcode$`Secondary Thorfare`)


NI_Pcode$`x/y coordinates` <- paste(NI_Pcode$`x-coordinates`, ",", NI_Pcode$`y-coordinates`)

#NI_Pcode$`Locality/ Townland` <- paste(NI_Pcode$`Locality``, ",", `NI_Pcode$Townland``)


# reordering the dataset
NI_Pcode= NI_Pcode %>% select(`Primary Key (identifier)`,`Organisation Name`, `Sub-Building Name`
                                       , `Building Name`, Number, `Primary Thorfare`, `Alt/Secondary Thorfare`
                                       ,Locality,Townland, Town, County, Postcode, `x/y coordinates`)



NI_Pcode[NI_Pcode == ""] <- NA      #any cells with no value get replaced with NA
NI_Pcode[NI_Pcode == "NA,NA"] <- NA   #any cells containg NA, NA 
NI_Pcode[NI_Pcode == ","] <- NA       #cells containg jsut the seperator are replaced
NI_Pcode[NI_Pcode == " "] <- NA      #cells with a space seperator are replaced


# replacing NA values in Locality with value in Townland colum; makes most logical sense
# same for secondary thorfare with primary thorfare values
#NA values in organisation name field replaced with "RESIDENT"
#Sub-building and building left as is
NI_Pcode$Locality <- ifelse(is.na(NI_Pcode$Locality), NI_Pcode$Townland, NI_Pcode$Locality)

NI_Pcode$`Secondary Thorfare` <- ifelse(is.na(NI_Pcode$`Alt/Secondary Thorfare`), NI_Pcode$`Primary Thorfare`
                                            , NI_Pcode$`Alt/Secondary Thorfare`)

NI_Pcode$`Organisation Name` <- ifelse(is.na(NI_Pcode$`Organisation Name`), "RESIDENT", 
                                       NI_Pcode$`Organisation Name`)


# checking what number of rows contain NA values compared to the total number of rows in dataframe
nrow(NI_Pcode) 
sum(!complete.cases(NI_Pcode)) 

#results: every row (apart from 6) contains at least 1 NA value 
#removing all the NA values will result in scarce data


complete_rows <- na.omit(NI_Pcode)

# (d) (i) total NA values per col in dataset
NA_per_col <- colSums(is.na(NI_Pcode))
NA_per_col


# (d) (ii) mean NA values per col in dataset
meanNA_per_col <- colMeans(is.na(NI_Pcode))
meanNA_per_col

# (e) making county a categorising factor. (Assigning levels to each attribute)
class(NI_Pcode$County)
NI_Pcode$County <- factor(NI_Pcode$County)
nlevels(NI_Pcode$County)
levels(NI_Pcode$County)

# making limavady dataset
limavady <- subset(NI_Pcode,  Town == "LIMAVADY", select =`Locality`:`Town`)
limavady$Locality <- ifelse(is.na(limavady$Locality), limavady$Townland, limavady$Locality)

limavady$Locality <- as.factor(limavady$Locality)
str(limavady)
write.csv(limavady, 'C:\\Users\\Richard\\Desktop\\data_science\\CleanNIPostcodeData.csv', row.names = FALSE)


