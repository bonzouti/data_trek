#install.packages("sf")
#devtools::install_github("haozhu233/kableExtra")
#install.packages("PerformanceAnalytics")
library(tidyverse)
library(ggthemes)
library(scales)
library(GGally)
library(paletteer)
library(rmarkdown)
library(xaringan)
library(here)
library(ggpubr)
library(devtools)
library(maps)
library(mapproj)
library(rnaturalearth)
library(sf)
library(ggmap)
library(leaflet)
library(sp)
library(kableExtra)
library(corrplot)
library(PerformanceAnalytics)
library(patchwork)
library(descr)
here("data")
getwd()
setwd("C:/Users/PROF-ENS/Desktop/data_trek")

euka_ppe <- read.csv("data/euk_prok_stacked.csv")
view(euka_ppe)
#=======================#
#delete and rename columns
#=======================#
euka <- euka_ppe %>% 
  select(-Eukaryota, -X) %>% 
  rename(NPP = Non.photosynthetic_procaryotes, PP = Photosynthetic_procaryotes)
glimpse(euka)

data_raw <- pivot_longer(euka, 2:4,names_to = "Picoeukarytes", values_to = "abund")

data_raw

#density
p <- ggplot(data_raw, aes(x=log(abund),group= Picoeukarytes, color=Picoeukarytes)) + 
  geom_density(adjust = 1.5)+
  #viridis::scale_fill_viridis(discrete=TRUE) +
  #viridis::scale_color_viridis(discrete=TRUE)+
  scale_fill_manual(values=c("darkorange", "green", "purple"))+
  labs(title = "Proportion of PPEs and the overall picophytoplankton",
       x = "Abundence %",
      
       fill = "Picoeukarytes"
       
  )
  #facet_wrap(~types)
p
#=======================#
#Visualization data
#=======================#
data_raw %>% 
ggplot( aes(x=Picoeukarytes, fill=Picoeukarytes, )) +
  geom_histogram(binwidth = 1000) +
  scale_fill_manual(values=c("darkorange", "green", "purple")) 
  #theme_ipsum() +
  #labs(fill="")

#=======================#
#Boxplot visualization
#=======================#

ggplot(
  data = data_raw,
  mapping = aes(
    x = forcats::fct_reorder(types, abund),
    y = abund
  )
) +
  geom_boxplot()

#=======================#
# histogram
#=======================#

p <- ggplot(
  data = data_raw,
  mapping = aes(
    y = abund,
    x = forcats::fct_reorder(types, abund),
    fill = types
  )
) +
  geom_col()
p

ggsave("results/box_plot.png", p, width = 5.97, height = 4.79, dpi = 300)

#=============================================================================#   
#transforming variables values in percentage to calculate relative abundance
#======================p=======================================================#

all_ppe <- euka %>% 
  mutate(NPP1 = NPP/(NPP + PP + PPE)*100,PP1 = PP/(NPP + PP + PPE)*100,PPE1 = PPE/(NPP + PP + PPE)*100 )
all_ppe

#========================#
#Remove unused columns
#========================#

ppe_clean <- all_ppe %>% 
  select(-NPP, -PP, -PPE)

#=====================#
# Rename new colunm
#=====================#
ppe_stacked <- ppe_clean %>% 
  rename(NPP = NPP1, PP = PP1, PPE = PPE1)
head(ppe_stacked)
#==================#
# Pivote Table
#==================#
data_pivot <- pivot_longer(ppe_stacked, 2:4,names_to = "Pp_types", values_to = "abundance")

head(data_pivot)

#density 

ggplot(data_pivot, aes(x= abundance,group= Pp_types, color=Pp_types)) + 
  geom_histogram(binwidth = .5)+
  scale_fill_manual(values = c("darkorange", "green", "purple")) 
  #viridis::scale_fill_viridis(discrete=TRUE) +
  #viridis::scale_color_viridis(discrete=TRUE)+
  labs(title = "Proportion of PPEs and the overall picophytoplankton",
       x = "abundence %",
       
       fill = "Picoeukarytes\nPp_types"
       
  )

#=================================#
#bar plot of relative abundance
#=================================#
fig1 <- ggplot(data_pivot, aes(fill = Pp_types, x = Samples_ID, y = abundance)) +
  geom_bar(stat = "identity", width = .5) +
  scale_fill_manual(values = c("darkorange", "green", "purple")) +
  labs(title = "Proportion of PPEs and the overall picophytoplankton",
       x = "Samples_ID",
       y = "Relative abundance (%)",
       fill = "Picoeukarytes\ntypes"
       
  )+
theme(axis.text.x = element_text(angle = 90))

fig1
ggsave("results/bar_plot.png", fig1, width = 5.97, height = 4.79, dpi = 300)

#density
p <- ggplot(data_pivot, aes(x=log(abundance), color=Pp_types)) + 
  geom_density(adjust = 1.5)+
  facet_wrap(~Pp_types)+
  xlab("log(Relative abundance) (%)")
p

#====================#
# Save CSV file
#===================#
write.csv(ppe_stacked, "data/pico_stacked.csv", row.names = FALSE )

#===============================#
# import pp_en_features file
#===============================#
pp_env <- read.csv("data/pp_env_features.csv")
view(pp_env)
ppe_env <- pp_env %>% 
  select(-Samples_ID, -Legend)
summary(ppe_env)

#Plot1

plot1 <- ggplot(
  data = ppe_env,
  aes(
    y = PPE,
    x = Depth,
  )
) +
  stat_smooth(method = lm, se = FALSE, color = "black", formula = y ~ x) +
  geom_point() +
  labs(
    x = "Depth (m)",
    y = "PPE abundance (%)"
  ) +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 350
  ) 


# Plot O2
plot2 <- ggplot(
  data = ppe_env,
  aes(
    y = PPE,
    x = O2,
  )
) +
  stat_smooth(method = lm, se = FALSE, color = "black", formula = y ~ x)+
  geom_point() +
  labs(
    x = "O2 (umol/L)",
    y = "PPE abundance (%)"
  ) +
  
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 13
  ) 

#Plot salinity
plot3 <- ggplot(
  data = ppe_env,
  aes(
    y = PPE,
    x = Salinity,
  )
) +
  stat_smooth(method = lm, se = FALSE, color = "black", formula = y ~ x)+
  geom_point() +
  labs(
    x = "Salinity (PSU)",
    y = "PPE abundance (%)"
  ) +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 35
  ) 

#Plot NO2
plot4 <- ggplot(
  data = ppe_env,
  aes(
    y = PPE,
    x = NO2,
  )
) +
  stat_smooth(method = lm, se = FALSE, color = "black", formula = y ~ x)+
  geom_point() +
  labs(
    x = "NO2 (umol/L)",
    y = "PPE abundance (%)"
  ) +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = .2
  ) 

#Plot NO2NO3
plot5 <- ggplot(
  data = ppe_env,
  aes(
    y = PPE,
    x = NO2NO3,
  )
) +
  stat_smooth(method = lm, se = FALSE, color = "black", formula = y ~ x)+
  geom_point() +
  labs(
    x = "NO2NO3 (umol/L)",
    y = "PPE abundance (%)"
  ) +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 10
  ) 


#Plot NO2NO3
plot6 <- ggplot(
  data = ppe_env,
  aes(
    y = PPE,
    x = PO4,
  )
) +
  stat_smooth(method = lm, se = FALSE, color = "black", formula = y ~ x)+
  geom_point() +
  labs(
    x = "PO4 (umol/L)",
    y = "PPE abundance (%)"
  ) +
  
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1
  ) 

figure <- ggarrange(plot1, plot2, plot3, plot4, plot5, plot6,
                    ncol = 2, nrow = 3)
figure
ggsave("results/ppe_features_plot.png", figure, width = 5.97, height = 4.79, dpi = 300)


# Modèles linéaires généralisés
head(ppe_env)
reg <- glm(PPE ~Depth + O2, family = gaussian(link="identity"), data = ppe_env)
summary(reg)
coefficients(reg)
par(mfrow = c(2,2))
plot(reg)
hist(residuals(reg))

ppe_env %>% 
  select(-PPE) %>% 
  mutate_if(is.factor, as.numeric) %>% 
  as.matrix() -> cor_ppe

corPpe <- cor(cor_ppe)

corPpe %>% 
  round(2) %>% 
  knitr::kable() %>% 
  kableExtra::kable_styling(font_size = 9)

corrplot(corPpe, type = "upper", order = "AOE", 
         tl.col = "black", tl.srt = 45, diag = FALSE)

#Les corrélations positives sont affichées en bleu et 
#Les corrélations négatives en rouge.
#L’intensité de la couleur et la taille des cercles sont
#proportionnelles aux coefficients de corrélation.
#A droite du corrélogramme, la légende de couleurs
#montre les coefficients de corrélation et les couleurs correspondantes.

chart.Correlation(ppe_env, histogram=TRUE, pch=19)




#====================#
#Regression linéaire
#====================#
lm1 <- lm(PPE ~ Depth, data = ppe_env)
lm1
par(mfrow = c(2,2))
plot(lm1)


par(mfrow=c(1,2))
coef(lm1)
plot(PPE ~Depth, data = ppe_env)
abline(lm1)
hist(residuals(lm1))
#test de Shapiro-Wilk et d'un test d'asymétrie (skewness)
shapiro.test(residuals(lm1))
library(e1071)
skewness(residuals(lm1)) #coefficient positif,distribution décalée à gauche de la médiane
#[1] 1.097413 assymétrie positive



#=====================================#
#Transfom dataframe in a longer form
#=====================================#
ppe_env_pivot <- 
  pivot_longer(ppe_env, 2:7, names_to = "param_var", values_to = "param_values" )

head(ppe_env_pivot) 
sum(is.na(ppe_env_pivot))


#===========================#
#geom_point Visualization
#===========================#

# 
# fig2 <- ggplot(
#   data = ppe_env_pivot,
#   aes(
#     y = PPE,
#     x = param_values,
#     color = param_var
#   )
# ) +
#   geom_point() +
#   facet_wrap(~param_var) +
#   facet_wrap(~param_var, scales = "free_x") +
#   facet_wrap(~param_var, scales = "free_y")
# fig2
# ggsave("/results/ppe_env_feature_point.png", fig2, width = 5.97, height = 4.79, dpi = 300)




#==================================#
# Import geospatial data file
#==================================#
pp_abundance <- read.csv("data/ppe_abundance.csv")
view(pp_abundance)
glimpse(pp_abundance)


###import map data
map_data <- read.csv2("data/mapping.csv")


# Recode name for column "Regions"
mapData_recode <- map_data %>% 
  mutate(Regions= recode(Regions,
  "Indian Ocean " = "IO ",
  "North Atlantic Ocean " = "NAO",
  "North Pacific Ocean " = "NPO",
  "South Atlantic Ocean " = "SAO",
  "South Pacific Ocean " = "SPO",
  "Southern Ocean " = "SO "
  ))
glimpse(mapData_recode)
# Dropping  no some Columns 
mapData1 <- mapData_recode %>% 
  select(-c(X,Mean_T1, Marine_pelagic_biomes_Longhurst_2007,NO2NO3,PO4,Mean_Nitrates_umol_per_L,
            NO2_umoll_c1,NO2_umolL_c2,Mean_Nitrates_umol_perLc1, Oxygene1,
            oxygene2, NO2NO3_range,NO2NO3_range1, Salinity_range1, Salinity_range2,
            Temperature.1, reads_count1,reads_count2, Mean_T2,Environmental_Feature))

view(mapData1)

# Rename long names columns in short names columns
mapData2 <- mapData1 %>% 
rename(latN = Latitude_deg_N, longE = Longitude_deg_E, depth = Sampling_depth_in_m, NO2NO3 = NO2NO3_umol_L,
       PO4 = PO4_umol_L, NO2 = NO2_umol_L, O2 = Mean_Oxygen_umol_per_kg, Salinity = Mean_Salinity, PPEs = PPEs...)
view(mapData2)
# filter NA data
anyNA(mapData2)

mapData2 %>% 
filter(if_any(everything(), ~ is.na(.x)))

# Deleted NA data

mapData2 <-mapData2[complete.cases(mapData2),]

view(mapData2)
# mapping distribution of PPES

# define a custom palette
pal <- colorNumeric(
  c("#E1F5C4", "#EDE574", "#F9D423", "#FC913A", "#FF4E50"),
  # colors depend on the count variable
  domain =mapData2$PPEs,
)
mapData2$latN <- as.numeric(mapData2$latN)
mapData2$longE <- as.numeric(mapData2$longE)


map_p <- leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(data = mapData2, lng = ~longE, lat = ~latN, color = ~pal(PPEs),  opacity = 0.65, radius = 10) %>% 
      leaflet::addLegend( # add a legend
      data = mapData2,
      pal = pal,
      values = ~PPEs,
      position = "topright",
      title = "PPES % :",
      opacity = 0.9
    )
map_p
  #ggtitle("Densité empirique du nombre de locations entre 17h et 18h en 2011 par temps dégagé")




