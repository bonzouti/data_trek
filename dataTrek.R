
library(rnaturalearth)
library(tidyverse)
library(ggthemes)
library(scales)
library(GGally)
library(paletteer)
library(rmarkdown)
library(xaringan)
library(here)
library("ggpubr")
here("data")
getwd()
setwd("C:/Users/PROF-ENS/Desktop/data_trek")

euka_ppe <- read.csv("data/euk_prok_stacked.csv")
euka_ppe
#=======================#
#delete and rename columns
#=======================#
euka <- euka_ppe %>% 
  select(-Eukaryota, -X) %>% 
  rename(NPP = Non.photosynthetic_procaryotes, PP = Photosynthetic_procaryotes)
euka

data_raw <- pivot_longer(euka, 2:4,names_to = "types", values_to = "abund")

data_raw
#=======================#
#Visualization data
#=======================#
ggplot(
  data = data_raw,
  mapping = aes(
    x = abund,
    y = Samples_ID, fill=types
  )
) +
  geom_col()

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
    x = abund,
    y = forcats::fct_reorder(types, abund),
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

data_pivot

#=================================#
#bar plot of relative abundance
#=================================#
fig1 <- ggplot(data_pivot, aes(fill = Pp_types, x = Samples_ID, y = abundance)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("darkorange", "green", "purple")) +
  labs(title = "Proportion of PPEs and the overall picophytoplankton",
       x = "Samples_ID",
       y = "Relative abundance (%)",
       fill = "Picoeukarytes\ntypes"
  )
fig1
ggsave("results/bar_plot.png", fig1, width = 5.97, height = 4.79, dpi = 300)



#====================#
# Save CSV file
#===================#
write.csv(ppe_stacked, "data/pico_stacked.csv", row.names = FALSE )

#===============================#
# import pp_en_features file
#===============================#
pp_env <- read.csv("data/pp_env_features.csv")
head(pp_env)
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


reglinaire <- lm(PPE ~ Depth, data = ppe_env)
summary(reglinaire)

#=====================================#
#Transfom dataframe in a longer form
#=====================================#
ppe_env_pivot <- 
  pivot_longer(ppe_env, 2:7, names_to = "param_var", values_to = "param_values" )

ppe_env_pivot 
sum(is.na(ppe_env_pivot))


#===========================#
#geom_point Visualization
#===========================#


fig2 <- ggplot(
  data = ppe_env_pivot,
  aes(
    y = PPE,
    x = param_values,
    color = param_var
  )
) +
  geom_point() +
  facet_wrap(~param_var) +
  facet_wrap(~param_var, scales = "free_x") +
  facet_wrap(~param_var, scales = "free_y")
fig2
ggsave("/results/ppe_env_feature_point.png", fig2, width = 5.97, height = 4.79, dpi = 300)

#==================================#
# Import geospatial data file
#==================================#
pp_abundance <- read.csv("data/ppe_abundance.csv")
head(pp_abundance)
glimpse(pp_abundance)