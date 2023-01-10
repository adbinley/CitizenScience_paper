library(tidyverse)
library(ggrepel)


# data_countries <- read.table("data/wos_country.txt", fill = TRUE, header=TRUE, sep = "\t")

#going to have to clean this up if you want to use it


load("data/data_clean_nov10.RData")

# citsci_data <- read.csv("data/Theobald_data_clean.csv")

#https://www.dcceew.gov.au/science-research/abrs/publications/other/numbers-living-species/contents
#from Chapman et al 2009

dspecies <- data.frame(taxa_clean = unique(data_clean1$taxa_clean),
                       number_described = c(1359367,
                       9990,5487,NA,6515,297857,8734,98998))

# citsci_data1 <- citsci_data %>%
#   drop_na(Taxa_clean)
dspecies1 <- dspecies %>%
  drop_na(number_described)

#### taxa ####

n_art <- n_distinct(data_clean1$Article)

taxa_data_lit <- data_clean1 %>%
  group_by(taxa_clean) %>%
  summarise(count_lit = n_distinct(Article),
            proportion = n_distinct(Article)/n_art)

# n_cs <- length(citsci_data1$ï..Program.Name)

# taxa_data_cs <- citsci_data1 %>%
#   group_by(Taxa_clean) %>%
#   summarise(count_cs = length(Taxa_clean),
#             proportion_cs = length(Taxa_clean)/n_cs)

dspecies1$proportion_described <- dspecies1$number_described/sum(dspecies1$number_described)

# names(taxa_data_cs) <- c("taxa_clean","count_cs",  "proportion_cs")

comp_data <- left_join(taxa_data_lit, dspecies1, by = "taxa_clean")

10/sum(taxa_data_lit$count_lit)

# comp_data$taxa_clean <- gsub("all", "multi-taxa", comp_data$taxa_clean)
comp_data <- comp_data %>%
  drop_na(number_described)

res <- chisq.test(x = comp_data$count_lit, p = comp_data$proportion_described, correct = F)

res$expected

res_1 <- data.frame("taxon" = comp_data$taxa_clean,
                    "observed" = res$observed,
                    "expected" = res$expected)

comp_plot <- ggplot(res_1, aes(y=observed, x=expected, label = taxon))+
  geom_point()+
  #geom_text(hjust=0, vjust=-0.5)+
  geom_label_repel(aes(label = taxon, size = NULL), nudge_y = 0.7)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  theme_classic(base_size = 28, base_family = "serif")+
  labs(x="Expected Number of Articles", y="Observed Number of Articles")

png("fig_xsq1.png", height = 9, width = 9, units="in", res = 300)
comp_plot
dev.off()


#### geography ####

n_cs <- length(citsci_data1$ï..Program.Name)

geo_data_cs <- citsci_data1 %>%
  group_by(continent) %>%
  summarise(count_cs = length(continent),
            proportion_cs = length(continent)/n_cs)


n_art <- n_distinct(data_clean1$Article)

geo_data_lit <- data_clean1 %>%
  group_by(Continent) %>%
  summarise(count_lit = n_distinct(Article),
            proportion = n_distinct(Article)/n_art)

names(geo_data_lit) <- c("continent","count_lit",  "proportion_lit")

geo_comp_data <- left_join(geo_data_lit, geo_data_cs, by = "continent")

geo_comp_data1 <- geo_comp_data %>%
  drop_na(count_cs)

geo_comp_data1 <- geo_comp_data1 %>%
  mutate(proportion_cs = count_cs/sum(count_cs))


res_geo <- chisq.test(x = geo_comp_data1$count_lit, p = geo_comp_data1$proportion_cs, correct = F)

res_geo$expected

res_geo_1 <- data.frame("continent" = geo_comp_data1$continent,
                    "observed" = res_geo$observed,
                    "expected" = res_geo$expected)

geo_comp_plot <- ggplot(res_geo_1, aes(y=observed, x=expected, label = continent))+
  geom_point()+
  #geom_text(hjust=0, vjust=-0.5)+
  geom_label_repel(aes(label = continent, size = NULL), nudge_y = 0.7)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  theme_classic(base_size = 28, base_family = "serif")+
  labs(x="Expected Number of Articles", y="Observed Number of Articles")

geo_comp_plot

png("fig_xsq_cont.png", height = 9, width = 9, units="in", res = 300)
geo_comp_plot
dev.off()



#fisher exact test

# fexact_data <- comp_data %>%
#   select(c(taxa_clean,count_lit,count_cs))

# fexact_data <- data.frame("count_lit"=comp_data$count_lit,
#                           "count_cs"=comp_data$count_cs,
#                           row.names = comp_data$taxa_clean)
# 
# test1 <- fisher.test(fexact_data)
