library("xlsx")

# Auxiliary lib
source("rquery.cormat.R")

# PT data
pt.data <- read.xlsx("PT data.xlsx", 1)

# Demographic data
dem.data <- read.xlsx("Datos_SPSS_paperTDD_English.xlsx", 1)

# The pt.data contains several row, each one for a project file
# Let's group by to have just one row per subject
pt.data <- aggregate(formula = insertions ~ subject + project,
                     data = pt.data,
                     FUN = sum)

# Merge both datasets
all.pt.data <- merge(pt.data, dem.data, by.x = "subject", by.y = "SUBJECT_ID")
remove(pt.data)
remove(dem.data)

# Code analysis make sense by project
# Later, the experimental session could be considered
all.pt.data.about.mr <- all.pt.data[all.pt.data$project == "MR",]

# Some analysis
with(all.pt.data.about.mr, 
     {
       
       boxplot(insertions ~ CS_TITLE)
       
       plot(EXPERIENCE_EXPERIMENT_PROGRAMMING_LANGUAGE_ACADEMY_YEARS, insertions)
       abline(lm(insertions ~ EXPERIENCE_EXPERIMENT_PROGRAMMING_LANGUAGE_ACADEMY_YEARS))
       
       plot(EXPERIENCE_EXPERIMENT_PROGRAMMING_LANGUAGE_INDUSTRY_YEARS, insertions)
       abline(lm(insertions ~ EXPERIENCE_EXPERIMENT_PROGRAMMING_LANGUAGE_INDUSTRY_YEARS))
       
       plot(OVERALL_EXPERIENCE_PROGRAMMING_ACADEMY_YEARS, insertions)
       abline(lm(insertions ~ OVERALL_EXPERIENCE_PROGRAMMING_ACADEMY_YEARS))
       
       plot(OVERALL_EXPERIENCE_PROGRAMMING_INDUSTRY_YEARS, insertions)
       abline(lm(insertions ~ OVERALL_EXPERIENCE_PROGRAMMING_INDUSTRY_YEARS))
       
       plot(EXPERIENCE_TRADITIONAL_METHODS_INDUSTRY_YEARS, insertions)
       abline(lm(insertions ~ EXPERIENCE_TRADITIONAL_METHODS_INDUSTRY_YEARS))
       
       plot(EXPERIENCE_AGILE_METHODS_INDUSTRY_YEARS, insertions)
       abline(lm(insertions ~ EXPERIENCE_AGILE_METHODS_INDUSTRY_YEARS))

     }
     
)

# And the correlation among demographic variables (all variables, in fact)
numeric_only <- unlist(lapply(all.pt.data.about.mr, is.numeric))
rquery.cormat(all.pt.data.about.mr[ , numeric_only], type="flatten", graph=FALSE, greater.than.or.equal=0.5)


