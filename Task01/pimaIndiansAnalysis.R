library(caret)
library(ggplot)
library(ggcorrplot)

diabetes = read.csv("/Users/vanessa.vieira/Downloads/diabetes.csv")
str(diabetes)

# Pregnancies analysis
hist(diabetes$Pregnancies)

# Insulin analysis
hist(diabetes$Insulin)

# Diabetes pedigree function analysis
hist(diabetes$DiabetesPedigreeFunction)

# Age analysis
hist(diabetes$Age)

# Skin Tchickness analysis
hist(diabetes$SkinThickness)

skin_thickness_median <- median(diabetes$SkinThickness)

diabetes$SkinThickness[diabetes$SkinThickness == 0]<-skin_thickness_median

# Glucose analysis
hist(diabetes$Glucose)

glucose_median <- median(diabetes$Glucose)

diabetes$Glucose[diabetes$Glucose == 0]<-glucose_median

# BMI analysis
hist(diabetes$BMI)

bmi_median <- median(diabetes$BMI)

diabetes$BMI[diabetes$BMI == 0]<-bmi_median

# Blood Pressure analysis
hist(diabetes$BloodPressure)

blood_pressure_median <- median(diabetes$BloodPressure)

diabetes$BloodPressure[diabetes$BloodPressure == 0]<-blood_pressure_median

# Correlation

res <- cor(diabetes, use="complete.obs", method="kendall")
round(res, 2)

# Evaluating the correlations
# We see that the Age and Pregnancies have a high correlation
# comparing to the others (0.46)

ggplot(diabetes, aes(x=diabetes$Age, y=diabetes$Pregnancies, size=diabetes$Age)) + 
  geom_point()

ggplot(diabetes,aes(x=diabetes$Age,y=diabetes$Pregnancies, size=diabetes$Age))+
  geom_jitter(alpha=0.6)+
  labs(title="Age and Pregnancies scatterplot")

ggplot(diabetes,aes(x=diabetes$Age,fill=factor(diabetes$Outcome)))+
  geom_density(alpha=0.4)+scale_fill_manual(values=c("red", "blue"))+
  labs(title="")

# Building the correlogram 

corr<-round(cor(diabetes),1)
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("red", "white", "blue"), 
           title="Correlogram of Diabetes data", 
           ggtheme=theme_bw)

