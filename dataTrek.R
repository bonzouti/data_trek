library(tidyverse)
library(ggthemes)
library(scales)
library(GGally)
library(paletteer)
library(rmarkdown)
library(xaringan)

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

ggsave("C:\\Users\\PROF-ENS\\Desktop\\data_trek\\results\\box_plot.png", p, width = 5.97, height = 4.79, dpi = 300)

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
ggsave("C:\\Users\\PROF-ENS\\Desktop\\data_trek\\results\\bar_plot.png", fig1, width = 5.97, height = 4.79, dpi = 300)



#====================#
# Save CSV file
#===================#
write.csv(ppe_stacked, "C:\\Users\\PROF-ENS\\Desktop\\data_trek\\data\\pico_stacked.csv", row.names = FALSE )

#===============================#
# import pp_en_features file
#===============================#
pp_env <- read.csv("data/pp_env_features.csv")
ppe_env <- pp_env %>% 
  select(-Samples_ID, -Legend)
ppe_env

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
ggsave("C:\\Users\\PROF-ENS\\Desktop\\data_trek\\results\\ppe_env_point.png", fig2, width = 5.97, height = 4.79, dpi = 300)

#==================================#
# Import geospatial data file
#==================================#
pp_abundance <- read.csv("data/ppe_abundance.csv")
head(pp_abundance)
glimpse(pp_abundance)
