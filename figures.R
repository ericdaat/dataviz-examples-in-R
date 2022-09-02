# Librairies -------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggfittext)
library(ggridges)
library(hrbrthemes)  # https://github.com/hrbrmstr/hrbrthemes
library(ggstream)

hrbrthemes::import_roboto_condensed() 

# Configuration variables ------------------------------------------------------

BASE_SIZE <- 12
DPI <- 100

default_theme <- list(
  theme_ipsum_rc(grid = "X", 
                 base_size = BASE_SIZE,
                 plot_title_size = BASE_SIZE + 2,
                 axis_title_size = BASE_SIZE + 1)
)

flipped_theme <- list(
  coord_flip(),
  theme_ipsum_rc(grid = "X", 
                 base_size = BASE_SIZE,
                 plot_title_size = BASE_SIZE + 2,
                 axis_title_size = BASE_SIZE + 1),
  scale_y_continuous(labels=function(x) format(x, 
                                               big.mark = ",", 
                                               scientific = FALSE))
)

non_flipped_theme <- list(
  theme_ipsum_rc(grid = "Y", 
                 base_size = BASE_SIZE,
                 plot_title_size = BASE_SIZE + 2,
                 axis_title_size = BASE_SIZE + 1),
  scale_y_continuous(labels=function(x) format(x, 
                                               big.mark = ",", 
                                               scientific = FALSE))
)

# Read dataset -----------------------------------------------------------------

# https://public.opendatasoft.com/explore/dataset/vehicules-commercialises/table/?flg=fr&sort=puissance_maximale
df <- read.csv("./data/vehicules-commercialises.csv", sep=";")

df[df==""] <- NA  # fill empty strings with NA

df <- df[df$Année < 2015,]

# Number of cars by manufacturer -----------------------------------------------
fig1 <- df %>% 
  group_by(Marque) %>% 
  summarise(n=n()) %>% 
  top_n(n=10, wt=n) %>% 
  ggplot(aes(x=reorder(Marque, n),
             y=n,
             label=format(n, big.mark = ","))) +
    geom_bar(stat="identity") +
    geom_bar_text() +
    flipped_theme +
    theme(aspect.ratio = 1) +
    labs(title="Number of cars by the top 10 manufacturers",
         subtitle = "Mercedes and Volkswagen are the leading car manufacturers",
         y="Number of cars",
         x="Manufacturer")

ggsave(plot = fig1,
       dpi=DPI,
       path = "./figures",
       filename = "fig1.png")

fig2 <- df %>% 
  filter(Hybride=="Hybride") %>% 
  group_by(Marque) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=reorder(Marque, n),
             y=n,
             label=format(n, big.mark = ","))) +
  geom_point() +
  geom_segment(aes(x=reorder(Marque, n), 
                   xend=reorder(Marque, n), 
                   y=0, 
                   yend=n)) +
  geom_text(hjust=-.25) +
  flipped_theme +
  theme(aspect.ratio = 1) +
  ylim(0, 400) +
  labs(title="Number of hybrid cars by manufacturers",
       subtitle = "Lexus and Mercedes are the leading hybrid car manufacturers",
       y="Number of cars",
       x="Manufacturer")

ggsave(plot = fig2,
       dpi=DPI,
       path = "./figures",
       filename = "fig2.png")


# Car line distribution --------------------------------------------------------
fig3 <- df %>% 
  group_by(Marque, Gamme) %>% 
  summarise(n=n()) %>% 
  group_by(Marque) %>% 
  mutate(total=sum(n),
         ratio=100*(n/total)) %>% 
  filter(total>3200) %>% 
  ggplot(aes(x=reorder(Marque, total),
             y=ratio,
             fill=Gamme,
             label=round(ratio, 0))) +
    geom_bar(stat="identity") +
    geom_bar_text(position = "stack", place="right") +
    scale_fill_ipsum(na.value="grey") +
    flipped_theme +
    labs(title="Car line distribution, by the top 10 manufacturers",
         subtitle = "Mercedes and Volkswagen are the leading car manufacturers",
         y="Percentage of cars",
         x="Manufacturer",
         fill="Car line")

ggsave(plot = fig3,
       dpi=DPI,
       path = "./figures",
       filename = "fig3.png")

  
# Number of manufactured cars --------------------------------------------------
fig4 <- df %>% 
  group_by(Année) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=Année,
             y=n,
             label=sprintf("%sK", round(n/1000)))) +
    geom_bar(stat="identity") +
    geom_bar_text(place="top") +
    non_flipped_theme +
    scale_fill_ipsum(na.value="grey") +
    labs(title="Number of manufactured cars, yearly",
         subtitle="The number of manufactured cars increased from 6,000 to 55,000 in 15 years.",
         x="Year",
         y="Number of manufactured cars")

ggsave(plot = fig4,
       dpi=DPI,
       path = "./figures",
       filename = "fig4.png")

# Percentage of cars by fuel type ----------------------------------------------
fig5 <- df %>% 
  filter(Carburant %in% c("Essence", "Diesel")) %>% 
  group_by(Année, Carburant) %>% 
  summarise(n=n()) %>% 
  group_by(Année) %>% 
  mutate(total=sum(n),
         ratio=100*n/total) %>% 
  ggplot(aes(x=Année,
             y=ratio,
             fill=Carburant,
             label=round(ratio))) +
  geom_bar(stat="identity") +
  geom_bar_text(position="stack", place="top") +
  non_flipped_theme +
  scale_fill_ipsum(na.value="grey") +
  labs(title="Percentage of cars by fuel type, yearly",
       subtitle = "The percentage of diesel cars increased from 23% to 91% in 15 years",
       y="Percentage of cars",
       x="Year",
       fill="Fuel type")

ggsave(plot = fig5,
       dpi=DPI,
       path = "./figures",
       filename = "fig5.png")
   
# Fuel consumption evolution
fig6 <- df %>% 
  ggplot(aes(x=as.factor(Année),
             y=Consommation.mixte)) +
    geom_violin() +
    geom_text(data = df %>% 
                group_by(Année) %>% 
                summarise(median=median(Consommation.mixte, na.rm = T)),
              aes(x=as.factor(Année),
                  y=26,
                  label=round(median, 1))) +
    non_flipped_theme +
    ylim(0, 30) +
    labs(title="Fuel consumption distribution, yearly",
         subtitle = "The median fuel consumption is slowly decreasing and narrowing yearly",
         y="Fuel consumption",
         x="Year")

ggsave(plot = fig6,
       dpi=DPI,
       path = "./figures",
       filename = "fig6.png")

# Fuel consumption by manufacturer ---------------------------------------------
fig7 <- df %>% 
  group_by(Marque) %>% 
  mutate(median_consumption=median(Consommation.mixte, na.rm = T)) %>% 
  filter(median_consumption>10) %>% 
  ggplot(aes(x=reorder(Marque, median_consumption),
             y=Consommation.mixte)) +
    geom_violin() +
    coord_flip() +
    flipped_theme +
    ylim(0, NA) +
    labs(title="Manufacturers with high fuel consumptions",
         subtitle = "Bentley, Ferrari and Lamborghini manufacture high consumption vehicles",
         y="Fuel consumption",
         x="Manufacturer")

ggsave(plot = fig7,
       dpi=DPI,
       path = "./figures",
       filename = "fig7.png")
    
fig8 <- df %>% 
  group_by(Marque) %>% 
  mutate(median_consumption=median(Consommation.mixte, na.rm = T)) %>% 
  filter(median_consumption<7) %>% 
  ggplot(aes(x=reorder(Marque, median_consumption),
             y=Consommation.mixte)) +
  geom_violin() +
  coord_flip() +
  flipped_theme +
  ylim(0, NA) +
  labs(title="Manufacturers with low fuel consumptions",
       subtitle = "Smart, Dacia and Renault Tech manufacture low consumption vehicles",
       y="Fuel consumption",
       x="Manufacturer")

ggsave(plot = fig8,
       dpi=DPI,
       path = "./figures",
       filename = "fig8.png")

# CO2 distribution -------------------------------------------------------------
fig9 <- df %>% 
  ggplot(aes(x=CO2)) +
  geom_histogram(bins=50, 
                 color="black") +
  non_flipped_theme +
  xlim(0, NA) +
  labs(title="CO2 emissions distribution",
       subtitle="The median C02 emissions was 213 gCO2/km",
       x="CO2 emissions (gCO2/km",
       y="Number of cars")

ggsave(plot = fig9,
       dpi=DPI,
       path = "./figures",
       filename = "fig9.png")

# CO2 vs. number of cars vs car line -------------------------------------------
fig10 <- df %>% 
  group_by(Gamme) %>% 
  summarise(n=n(),
            median_power=median(Puissance.administrative, na.rm=T),
            median_CO2=median(CO2, na.rm = T)) %>% 
  ggplot(aes(x=median_power,
             y=median_CO2,
             size=n,
             fill=Gamme)) +
    geom_point(shape=21) +
    xlim(0, NA) +
    ylim(75, NA) +
    default_theme +
    scale_fill_ipsum() +
    scale_size_continuous(breaks = c(1000, 25000, 50000, 100000),
                          range = c(3, 6),
                          labels=function(x) format(x, 
                                                    big.mark = ",", 
                                                    scientific = FALSE)) +
    guides(fill=guide_legend(override.aes = list(size = 6))) +
    labs(title="CO2 emissions vs. power",
         subtitle="Median CO2 emissions compared with median power, per car line",
         x="Power",
         y="CO2 emissions (gCO2/km",
         size="Number of cars",
         fill="Car line")

ggsave(plot = fig10,
       dpi=DPI,
       path = "./figures",
       filename = "fig10.png")

# CO2 emissions by fuel type ---------------------------------------------------
fig11 <- df %>% 
  filter(Carburant %in% c("Essence", "Diesel")) %>% 
  ggplot(aes(x=CO2,
             y=Carburant,
             fill=Carburant)) +
    geom_density_ridges(show.legend = F) +
    default_theme +
    scale_fill_ipsum() +
    xlim(0, NA) +
    labs(title="CO2 emissions distribution, by fuel type",
         subtitle="CO2 emissions were lower for Disel fuel",
         x="CO2 emissions",
         y="Fuel type")

ggsave(plot = fig11,
       dpi=DPI,
       path = "./figures",
       filename = "fig11.png")

# Evolution of the number of cars by line --------------------------------------
fig12 <- df %>% 
  filter(Marque %in% c("AUDI", "BMW", "MERCEDES", "VOLKSWAGEN")) %>% 
  group_by(Année, Marque) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=Année,
             y=n,
             fill=Marque)) +
    geom_stream() +
    non_flipped_theme +
    scale_fill_ipsum() +
    labs(title="Evolution of the number of cars by German manufacturers",
         subtitle="Mercedes growth happened in the early 2000s, VW later after 2010",
         x="Year",
         y="Number of cars",
         fill="Manufacturer")

ggsave(plot = fig12,
       dpi=DPI,
       path = "./figures",
       filename = "fig12.png")

