# Load libraries, functions and data ----

## Libraries
library(tidyverse); library(readxl); library(writexl)
#library(psych); 
library(lm.beta); library(fastDummies); library(Hmisc)


# Working directory and original dataset
setwd("~/Documents/230628_MSCA_tables/READMEfiles/")
df <- read_excel("impact IR_2022-2023_20230402.xlsx")


# In house functions
source("Functions.R")

# Prepare the data for correlations & linear regression ----
## Factors according to SPSS loadings 
df$F1 <- df %>% dplyr::select(paste0("impact", c(3, 2, 29, 43, 46, 24, 19, 27, 33))) %>% rowMeans()
df$F2 <- df %>% dplyr::select(paste0("impact", c(21, 14, 10, 18, 44, 9, 48, 47, 23))) %>% rowMeans()
df$F3 <- df %>% dplyr::select(paste0("impact", c(41, 31, 34, 36, 42, 26))) %>% rowMeans()
df$F4 <- df %>% dplyr::select(paste0("impact", c(17, 37, 25, 40, 20, 7))) %>% rowMeans()
df$F5 <- df %>% dplyr::select(paste0("impact", c(4, 8, 16, 15))) %>% rowMeans()
df$F6 <- df %>% dplyr::select(paste0("impact", c(30, 12, 22, 32, 5))) %>% rowMeans()
df$F7 <- df %>% dplyr::select(paste0("impact", c(39, 35, 38, 45))) %>% rowMeans()


## Recode SECS questions and generate composite (Conservatism)
for (i in c(1,5)) {
  colname <- paste0("SECS_",i)
  oldcolname <- paste0(colname,"_old")
  df[oldcolname]=df[colname]  
  df[colname]  = abs(100-df[colname])
}
df$Conservatism <- round(rowMeans(df[, paste0("SECS_",c(1:12))], na.rm=TRUE))


## Recode the countries 
NorthSouth_coding <- read.csv("~/Documents/StuffForFa/FaData/230628_MSCA_tables/READMEfiles/NorthSouth_coding.csv")
NorthSouth_coding <- NorthSouth_coding %>% rename(nationality = Country, Nationality_South = South)
df <- df %>% left_join(NorthSouth_coding, by = 'nationality')


## Create a new dataframe for regression only (dummy variables make the dataset super long & impossible to read)
dfRg <- df


## Get age into numbers
dfRg$age <- case_when(
  df$age == "25-34" ~ 1,
  df$age == "35-44" ~ 2,
  df$age == '45-54' ~ 3,
  df$age == "55-64" ~ 4,
  df$age == "65 and over" ~ 5
)

## Get gender as a dummy
dfRg$gender <- case_when(
  dfRg$gender == 0 ~ 'Male',
  dfRg$gender == 1 ~ 'Female',
  dfRg$gender == 2 ~ 'Other'
)


## Remove space of long answers (easier to handle as they will become column name when transformed into dummy variables)
dfRg$area <- dfRg$area %>% str_remove_all(" ")
dfRg$IRtheory <- dfRg$IRtheory %>% str_remove_all(" ")
dfRg$employment <- dfRg$employment %>% str_remove_all(" ")
dfRg[dfRg == "InternationalRelationsofaParticularRegion/Country"] <- 
  "InternationalRelationsofaParticularRegion"
dfRg[dfRg == "InternationalOrganization(s)"] <- 
  "InternationalOrganization"


## Area (we have all the columns but we shouldn't load them all in linRg)
dfRg <- dummy_cols(dfRg, select_columns = c('IRtheory', 'area', 'employment', 'age', 'gender'))


# For Tenured/tenure track, the cancelled variable is non tenured 
  ## Though this dummy only works if person works in academia so we need to split academics to those whoe are not
dfRg$Tenured <- ifelse(dfRg$employment %in% 
                         c("AssociateProfessor","FullProfessor"), 1, 0)
dfRg$TenureTrack <- ifelse(dfRg$employment %in% 
                             c("AssistantProfessor"), 1, 0)

dfRg$NonAc <- ifelse(dfRg$employment %in% 
                       c("Government", 
                         "IndependentScholar", 
                         "PrivateorNGOSector"), 1, 0)

# Remove the people working in governement from the "Scholar" datasets
dfScholar <- dfRg %>% filter(employment != "Government")



# Correlation ----
dir.create('ResultFiles')
CorrelationVar <- c(paste0("F", c(1:7)), #the 7 factors of impact
                    "gender_Female", "Conservatism", "Tenured", 
                    "TenureTrack", "Nationality_South")
corrMatrix <- rcorr(as.matrix(dplyr::select(dfRg, CorrelationVar)))
Coefs <- star.pvalues(corrMatrix$P, corrMatrix$r)
## Format the result dataframe
for (i in 1:nrow(Coefs)) {
  for (j in 1:ncol(Coefs)) {
    if(j > i){Coefs[i,j] <- ""}
  }
}
Coefs[Coefs == "1"] <- "–"
colnames(Coefs) <- 1:ncol(Coefs)
## Export
write_xlsx(rownames_to_column(Coefs), "ResultFiles/Table2_Correlations.xlsx")

rm(corrMatrix, Coefs, CorrelationVar)




# Linear regressions ----
## Get the variables we are interested in in vectors
Factors <- paste0("F", c(1:7))
CoV <- c("gender_Female", "Nationality_South", "Conservatism")
Tenure <- c("Tenured", "TenureTrack")
JobTitle <- c("employment_AssistantProfessor", "employment_AssociateProfessor", 
              "employment_FullProfessor", "NonAc")
IRtheorylist <- dfRg %>% dplyr::select(contains("IRtheory_")) %>% names()
arealist <- dfRg %>% dplyr::select(contains("area_")) %>% names()

### Cancelled variables
IRtheorylist <- IRtheorylist[IRtheorylist!="IRtheory_Idonotuseparadigmaticanalysis"] #
arealist <- arealist[arealist!="area_IamnotanIRscholar"]

## Run linear regressions

### Table 3 - Job and tenure
res <- c("", Tenure, JobTitle, CoV, "R squared","Adjusted R squared", "N") %>% 
  as.data.frame(); names(res) <- "rowname"

for (i in 1:length(Factors)) {
  # model 1, age & gender (CoV)
  res <- modelLinReg(dfScholar, res, Factors[i], 
                   all_of(c(Tenure, CoV)), 1)
  # model 2, cov
  res <- modelLinReg(dfRg, res, Factors[i], 
                   all_of(c(JobTitle, CoV)), 2)
} 
write_xlsx(res, "ResultFiles/Table3_linearReg_Job.xlsx", col_names = FALSE)


### Table 4 - IR theory
res <- c("", IRtheorylist, "R squared","Adjusted R squared", "N") %>% 
  as.data.frame(); names(res) <- "rowname"

for (i in 1:length(Factors)) {
  # model 1
  res <- modelLinReg(dfRg, res, Factors[i], 
                   all_of(IRtheorylist), 1)
} 
write_xlsx(res, "ResultFiles/Table4_linearReg_IRtheory.xlsx", col_names = FALSE)



### Table 5 - Area of study
res <- c("", arealist, "R squared","Adjusted R squared", "N") %>% 
  as.data.frame(); names(res) <- "rowname"

for (i in 1:length(Factors)) {
  # model 1
  res <- modelLinReg(dfRg, res, Factors[i], 
                   all_of(arealist), 1)
} 
write_xlsx(res, "ResultFiles/Table5_linearReg_Area.xlsx", col_names = FALSE)



# Appendix ----
## Demographics
### Gender
res <- tibble(Demographics = "Female", 
              Percentage= sum(dfRg$gender_Female)/nrow(dfRg)*100)
res <- res %>% add_row()

### Age
tmp <- df$age %>% table() %>% as.data.frame()
tmp$Freq <- tmp$Freq/nrow(df)*100
names(tmp) <- c("Demographics", "Percentage")

res <- rbind(res, tmp)
res <- res %>% add_row()

### Education
res <- res %>% add_row(Demographics = "Education", Percentage= NA)
tmp <- df$education %>% table() %>% as.data.frame()
tmp$Freq <- tmp$Freq/nrow(df)*100
names(tmp) <- c("Demographics", "Percentage")

res <- rbind(res, tmp)
res <- res %>% add_row()

### Employment
res <- res %>% add_row(Demographics = "Employment", Percentage= NA)
tmp <- df$employment %>% table() %>% as.data.frame()
tmp$Freq <- tmp$Freq/nrow(df)*100
names(tmp) <- c("Demographics", "Percentage")

res <- rbind(res, tmp)
res$Percentage <- res$Percentage %>% round(digits = 1)

### Total number of people
res <- res %>% add_row()
res <- res %>% add_row(Demographics = "N", Percentage= nrow(dfRg))

write_xlsx(res, "ResultFiles/TableA1_Demographics.xlsx")


## Countries 
tmp <- df$nationality %>% table() %>% as.data.frame()
tmp$Perc <- tmp$Freq/nrow(df)*100
tmp <- tmp[,c(1,3,2)]
names(tmp) <- c("Country", "Percentage", "Absolute numbers")
tmp$Percentage <- tmp$Percentage %>% round(digits = 1)
write_xlsx(tmp, "ResultFiles/TableA2_Nationalitytable.xlsx")


## Conservatism density
ggplot(dfRg) + aes(Conservatism) + geom_density() + theme_classic()
ggsave('ResultFiles/FigureA1_ConservatismDensity.png', width = 5, height = 3)

## INFO SESSION
sessionInfo()
