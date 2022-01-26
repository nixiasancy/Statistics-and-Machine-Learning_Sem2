##################################### CHAPTER 2 ######################################

#################### Exercise 2 ############################

######### a) Factors affected CEO salary

### 1) It is a regression problem
### 2) We are more interested in inference since we would like to know the factors which affect rather than predicting the CEO's salary
### 3)

######## b) Success or Failure of product

### 1) It is a classification problem
### 2) We are interested in prediction to know if our product would succeed or fail
### 3)

######## c) % change in USD/Euro exchange

### 1) It is a regression problem
### 2) Interested in prediction
### 3)

#################### Exercise 10 ############################
# a)

library(MASS)
Boston <- Boston
head(Boston)
?Boston
## The data has 506 rows and 14 columns

## Rows represent each town and the columns represent various statistics of the towns

# b) GGPLOT
library(ggplot2)
ggplot() + geom_point(aes(x = Boston$indus, y = Boston$rad))
ggplot() + geom_point(aes(x = Boston$tax, y = Boston$nox))

# c) 
cor_crim_rate <- t(cor(Boston$crim,Boston[,2:14]))

cor_crim_rate <- as.data.frame(cor_crim_rate) %>%
                    arrange(cor_crim_rate,desc(V1))
col_names <- row.names(cor_crim_rate)
cor_crim_rate <- as.data.frame(cbind(col_names,cor_crim_rate$V1))
ggplot(cor_crim_rate) + geom_col(aes(x=col_names,y=V2)) +
  scale_y_discrete()

## index of accessibility to radial highways and tax has high positive correlation
## medv and black has high negative correlation to the crime rate

# d) 

## max crime rate 
row_names <- as.integer(row.names(Boston))
Boston_new <- rename(cbind(row_names,Boston),Town = row_names)
max_crime_rate <- Boston_new %>%
                    filter(crim == max(crim))

## high tax rate

high_tax_rate <- Boston_new %>%
                  filter(tax == max(tax))

## Pupil-teacher ration

high_ptratio <- Boston_new %>%
                  filter(ptratio == max(ptratio))

##
col_range <- t(sapply(Boston_new, range))

# e) 
town_by_river <- Boston_new %>%
                  filter(chas == 1) %>%
                      nrow()


##################################### CHAPTER 3 ######################################

#################### Exercise 3 ############################

#################### Exercise 10 ############################
# a)

install.packages("ISLR")
library(ISLR)
attach (Carseats)
install.packages("caret")

lm.fit = lm(Sales~Price+Urban+US,data = Carseats)
summary(lm.fit)

## b) 

# B1 (Price) - The co-efficient is negative and hence has inverse relation to Sales
# B2 (UrbanYes) - The co-efficient is negative and inversely proportional. But since the p-value is high it does not impact sales
# B3 (USYes) - The co-efficient is positive and have a direct relation to sales.

## c) Write out the model in equation form, being careful to handle the qualitative variables properly

# Y = 13.04 + (-0.054)*Price + (-0.021) * UrbanYes + 1.2*USYes

## d)
# We can reject null hypothesis for Price and USYes because p-value is very less

## e) 

lm2.fit = lm(Sales~Price + US, data = Carseats)
summary(lm2.fit)

## f)

# the adjusted r2 value increases on better features. The second model is better because it has higher adjusted r2.

## g) 

confint(lm2.fit, level = 0.95) # it is a two sided test and hence split with 2.5% on each side

## h)



#################### Exercise 15 ############################
## a) 

col_names

lm_fit_medv <- lm(crim ~ medv,data = Boston)
lm_fit_black <- lm(crim ~ black,data = Boston)
lm_fit_dis <- lm(crim ~ dis,data = Boston)
lm_fit_rm <- lm(crim ~ rm,data = Boston)
lm_fit_zn <- lm(crim ~ zn,data = Boston)
lm_fit_chas <- lm(crim ~ chas,data = Boston)
lm_fit_ptratio <- lm(crim ~ ptratio,data = Boston)
lm_fit_age <- lm(crim ~ age,data = Boston)
lm_fit_indus <- lm(crim ~ indus,data = Boston)
lm_fit_nox <- lm(crim ~ nox,data = Boston)
lm_fit_lstat <- lm(crim ~ lstat,data = Boston)
lm_fit_tax <- lm(crim ~ tax,data = Boston)
lm_fit_rad <- lm(crim ~ rad,data = Boston)

summary(lm_fit_medv)
summary(lm_fit_black)
summary(lm_fit_dis)
summary(lm_fit_rm)
summary(lm_fit_zn)
summary(lm_fit_chas)
summary(lm_fit_ptratio)
summary(lm_fit_age)
summary(lm_fit_indus)
summary(lm_fit_nox)
summary(lm_fit_lstat)
summary(lm_fit_tax)
summary(lm_fit_rad)

## The following predictors have significant impact on the response : medv,black,dis,rm,zn,ptratio,age,indus,nox,lstat,tax,rad

# b) 

lm_fit <- lm(crim ~ .,data = Boston)
summary(lm_fit)

# We can reject the null hypothesis for zn,dis,rad,black,medv

# c) 
# when using multiple features, fewer features have higher significance

# d)

lm_poly_medv <- lm(crim ~ poly(medv,3),data = Boston)
lm_poly_black <- lm(crim ~ poly(black,3),data = Boston)
lm_poly_dis <- lm(crim ~ poly(dis,3),data = Boston)
lm_poly_rm <- lm(crim ~ poly(rm,3),data = Boston)
lm_poly_zn <- lm(crim ~ poly(zn,3),data = Boston)
lm_poly_ptratio <- lm(crim ~ poly(ptratio,3),data = Boston)
lm_poly_age <- lm(crim ~ poly(age,3),data = Boston)
lm_poly_indus <- lm(crim ~ poly(indus,3),data = Boston)
lm_poly_nox <- lm(crim ~ poly(nox,3),data = Boston)
lm_poly_lstat <- lm(crim ~ poly(lstat,3),data = Boston)
lm_poly_tax <- lm(crim ~ poly(tax,3),data = Boston)
lm_poly_rad <- lm(crim ~ poly(rad,3),data = Boston)


summary(lm_poly_medv)
summary(lm_poly_black)
summary(lm_poly_dis)
summary(lm_poly_rm)
summary(lm_poly_zn)
summary(lm_poly_ptratio)
summary(lm_poly_age)
summary(lm_poly_indus)
summary(lm_poly_nox)
summary(lm_poly_lstat)
summary(lm_poly_tax)
summary(lm_poly_rad)


# medv, all three significant
# black only linear significant
# dis , all three are significant
# rm, linear and quadratic significance, cubic no
#zn, linear and quadratic significant , cubic no
# ptratio, all three significant
#age, all three significant
#indus, all three significant
#nox, all three are significant
# lstat, linear and quadratic are significant, cubic no
# tax, linear and quadratic are significant, cubic no
# rad, linear and quadratic are significant, cubic no