# Loading Libraries
library(sf)
library(ggplot2)
library(dplyr)
library(spdep)
library(broom)
library(spatialreg)
#Load London Data save in Ward variable
wards <- sf::st_read("LondonWards.shp")
wards1<- sf::st_read("LondonWards.shp")

#visualizing Ward data 
ggplot(data = wards) +
  geom_sf(aes(fill = AGc_2)) +
  scale_fill_viridis_c(direction = -1) +
  labs(fill='Average GCSE')

#Ploting values of median Age  2013  and Average GCSE
ggplot(data = wards, aes(x = MdA_2013, y = AGc_2)) +
  geom_point() +
  xlab("median Age  2013") +
  ylab("Average GCSE - 2014") +
  theme_minimal()

#Ploting Another Independent Variable Employment rate (16-64) - 2011 and Average GCSE
ggplot(data = wards, aes(x = E_16_, y = AGc_2)) +
  geom_point() +
  xlab("Employment rate (16-64)") +
  ylab("Average GCSE - 2014") +
  theme_minimal()

#Apply Linear Regression Method to see correlation between Employment and GSCE Score
ggplot(data = wards, aes(x = E_16_, y = AGc_2)) +
  geom_point() +xlab("Employment rate (16-64)") +
  ylab("Average GCSE - 2014") +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=FALSE) +
  theme_minimal()

# Summary of the linear model for Employment variable with GCSE Score
model <- lm(AGc_2 ~ E_16_, data=wards)
summary(model)

# Summary of the Multivariate model for two Independent variable with GCSE Score
multivariate <- lm(AGc_2 ~ MdA_2013+E_16_, data=wards)
summary(multivariate)

#Multicollinearity Test
cor.test(wards$MdA_2013, wards$E_16_, method = "pearson")


#Modelling Residual of Multivariate Model
res.df <- data.frame(res = multivariate$residuals)
ggplot(data = res.df, aes(x = res)) +
  geom_histogram() +xlab("Residual") +
  ylab("Count") +
  theme_minimal()

#Visualize residuals in map
wards <- wards %>%
  mutate(resids = res.df$res)
ggplot(data = wards) +
  geom_sf(aes(fill = resids)) +
  scale_colour_gradient2( low = "green", mid = "white",
                          high = "brown", midpoint = 0,
                          aesthetics = "fill") +
  labs(fill='MultiVariate Residuals')

#Spatial Lag Regression
coordsW <- st_geometry(wards1) %>%
  st_centroid()
LWard_nb2 <-coordsW %>%
  knearneigh(., k=4)

LWard_knn <- LWard_nb2 %>%
  knn2nb()
# run spatial lag model
slag_model_knn <- lagsarlm(AGc_2 ~MdA_2013+E_16_,
                           data = wards,
                           nb2listw(LWard_knn, style="C"))
tidy(slag_model_knn)

