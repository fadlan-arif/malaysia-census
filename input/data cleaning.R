install.packages("writexl")
library(writexl)
government_school_teachers <- read_excel("data/Number_of_government_school_teachers_by_District_Education_Office_and_by_State_2017-2018 (2).xlsx")

filtered_data <- government_school_teachers %>%
  filter(`Number of teachers` != '-', Year == '2018', `School type` != 'NA')

filtered_data2 <- filtered_data %>% slice(rep(1:n(), each = `Number of teachers`)) %>%
  select(Year, `School stage`, `School type`, State, `District Education Office`, Sex)

final_set <- filtered_data2 %>% mutate(bin_schooltype = if_else(`School type` == "Academic", 1, 0))

write_xlsx(final_set,"C:\\Users\\ariff\\OneDrive\\UTM Classes\\Year 3\\STA304\\malaysia-census\\input\\data\\final dataset.xlsx")

final_data <- read_excel("data/final dataset.xlsx")

#computing general linear model for dataset
first_logit <- glm(bin_schooltype ~ Sex + State, data = final_data, na.action = "na.exclude", family = 'binomial')

summary(first_logit)
#assigning variable names to each coefficient
b0 <- first_logit$coef[1] #intercept
sexMale <- first_logit$coef[2]
StateKedah  <- first_logit$coef[3]
StateKelantan  <- first_logit$coef[4]
StateMelaka  <- first_logit$coef[5]
StateNegeriSembilan  <- first_logit$coef[6]
StatePahang <- first_logit$coef[7]
StatePerak  <- first_logit$coef[8]
StatePerlis <- first_logit$coef[9]
StatePulauPinang  <- first_logit$coef[10]
StateSabah  <- first_logit$coef[11]
StateSarawak  <- first_logit$coef[12]
StateSelangor  <- first_logit$coef[13]
StateTerengganu  <- first_logit$coef[14]
StateW.P.KualaLumpur <- first_logit$coef[15]
StateW.P.Labuan  <- first_logit$coef[16]
StateW.P.Putrajaya  <- first_logit$coef[17]

