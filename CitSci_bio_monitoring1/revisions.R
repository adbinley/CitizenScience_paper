library(tidyverse)
library(ggrepel)


# data_countries <- read.table("data/wos_country.txt", fill = TRUE, header=TRUE, sep = "\t")

#going to have to clean this up if you want to use it


load("data/data_clean_nov10.RData")

# citsci_data <- read.csv("data/Theobald_data_clean.csv")

#https://www.dcceew.gov.au/science-research/abrs/publications/other/numbers-living-species/contents
#from Chapman et al 2009

dspecies <- data.frame(taxa_clean = unique(data_clean1$taxa_clean),
                       number_described = c(
                         1202119,#inverts
                       9990,#birds
                       5487, #mammals
                       NA,#multi-taxa
                       6515,#amphibian
                       297857,#plants
                       8734,#reptiles
                       98998))#fungi

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

comp_data$difference <- comp_data$proportion - comp_data$proportion_described

comp_plot <- ggplot(res_1, aes(y=observed, x=expected, label = taxon))+
  geom_point()+
  #geom_text(hjust=0, vjust=-0.5)+
  geom_label_repel(size = 7,aes(size = NULL), nudge_y = 0.7)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  theme_classic(base_size = 24, base_family = "sans")+
  labs(x="Expected Number of Articles", y="Observed Number of Articles")

png("figures/fig_xsq1.png", height = 10, width = 10, units="in", res = 300)
comp_plot
dev.off()



#### data deficient species ####

dds <- read.csv("data/redlist_data/assessments.csv")

dds <- dds %>%
  select(c("scientificName","redlistCategory"))

our_sps <- unique(data_clean1$species_level)

our_dd_sps <- our_sps %in% dds$scientificName

our_dd_sps_df <- data.frame(species = our_sps,
                            dd = our_dd_sps)


#### r scopus ####

install.packages("rscopus")
library(rscopus)

set_api_key("d6655768243098762322155806c1e7f0")

# scopus search string: 
# TITLE-ABS-KEY ( {community based participatory research}  OR  {community-based participatory research}  OR  "public participation"  OR  hunter  OR  naturalist  OR  atlas*  OR  amateur  OR  "citizen researcher"  OR  {individual citizen scientist}  OR  collaborator  OR  {lay knowledge holder} )  AND  TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY (species)  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer ) 

# taxonomic 

bird_cs <- scopus_search(query="TITLE-ABS-KEY ( {community based participatory research}  OR  {community-based participatory research}  OR  {public participation}  OR  hunter  OR  naturalist  OR  atlas*  OR  amateur  OR  {citizen researcher}  OR  {individual citizen scientist}  OR  collaborator  OR  {lay knowledge holder} )  AND  TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY (bird* OR avian)  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer )") #732 cs bird papers

bird <- scopus_search(query="TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY (bird* OR avian)  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer)") #30897 bird papers


invertebrate_cs <- scopus_search(query="TITLE-ABS-KEY ( {community based participatory research}  OR  {community-based participatory research}  OR  {public participation}  OR  hunter  OR  naturalist  OR  atlas*  OR  amateur  OR  {citizen researcher}  OR  {individual citizen scientist}  OR  collaborator  OR  {lay knowledge holder} )  AND  TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY (invert*)  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer )") # 82 cs inv papers

invertebrate <- scopus_search(query="TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY (invert*)  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer)") # 13866


mammal_cs <- scopus_search(query="TITLE-ABS-KEY ( {community based participatory research}  OR  {community-based participatory research}  OR  {public participation}  OR  hunter  OR  naturalist  OR  atlas*  OR  amateur  OR  {citizen researcher}  OR  {individual citizen scientist}  OR  collaborator  OR  {lay knowledge holder} )  AND  TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY (mammal*)  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer )") # 361

mammal <- scopus_search(query="TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY (mammal*)  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer)") # 24705

plant_cs <- scopus_search(query="TITLE-ABS-KEY ( {community based participatory research}  OR  {community-based participatory research}  OR  {public participation}  OR  hunter  OR  naturalist  OR  atlas*  OR  amateur  OR  {citizen researcher}  OR  {individual citizen scientist}  OR  collaborator  OR  {lay knowledge holder} )  AND  TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY (plant*)  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer )") #654

plant <- scopus_search(query="TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY (plant*)  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer)") #124380


amphibian_cs <- scopus_search(query="TITLE-ABS-KEY ( {community based participatory research}  OR  {community-based participatory research}  OR  {public participation}  OR  hunter  OR  naturalist  OR  atlas*  OR  amateur  OR  {citizen researcher}  OR  {individual citizen scientist}  OR  collaborator  OR  {lay knowledge holder} )  AND  TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY (amphib*)  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer )") #89

amphibian <- scopus_search(query="TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY (amphib*)  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer)") #7794


reptile_cs <- scopus_search(query="TITLE-ABS-KEY ( {community based participatory research}  OR  {community-based participatory research}  OR  {public participation}  OR  hunter  OR  naturalist  OR  atlas*  OR  amateur  OR  {citizen researcher}  OR  {individual citizen scientist}  OR  collaborator  OR  {lay knowledge holder} )  AND  TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY (reptil*)  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer )") #99

reptile <- scopus_search(query="TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY (reptil*)  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer)") #4867


fungi_cs <- scopus_search(query="TITLE-ABS-KEY ( {community based participatory research}  OR  {community-based participatory research}  OR  {public participation}  OR  hunter  OR  naturalist  OR  atlas*  OR  amateur  OR  {citizen researcher}  OR  {individual citizen scientist}  OR  collaborator  OR  {lay knowledge holder} )  AND  TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY (fung*)  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer )") #38

fungi <- scopus_search(query="TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY (fung*)  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer)") #14990


taxon_comp <- data.frame(taxa_clean = c("invertebrate","bird","mammal","amphibian","plant","reptile","fungi"),
                         CS = c(82,732,361,89,654,99,38),
                         all_lit = c(13866,30897,24705,7794,124380,4867,14990))

taxon_comp$proportion_lit <- taxon_comp$all_lit/sum(taxon_comp$all_lit)

res2 <- chisq.test(x = taxon_comp$CS, p = taxon_comp$proportion_lit, correct = F)

res2$expected

res2_summary <- data.frame("taxon" = taxon_comp$taxa_clean,
                    "observed" = res2$observed,
                    "expected" = res2$expected)

#taxon_comp$difference <- taxon_comp$proportion - taxon_comp$proportion_lit

taxon_comp_plot <- ggplot(res2_summary, aes(y=observed, x=expected, label = taxon))+
  geom_point()+
  #geom_text(hjust=0, vjust=-0.5)+
  geom_label_repel(size = 7,aes(size = NULL), nudge_y = 0.7)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  theme_classic(base_size = 24, base_family = "sans")+
  labs(x="Expected Number of Articles", y="Observed Number of Articles")

png("figures/fig_xsq2.png", height = 10, width = 10, units="in", res = 300)
taxon_comp_plot
dev.off()

taxon_comp$CS_prop <- taxon_comp$CS/taxon_comp$all_lit


# geography #

North_America_cs <- scopus_search(query="TITLE-ABS-KEY ( {community based participatory research}  OR  {community-based participatory research}  OR  {public participation}  OR  hunter  OR  naturalist  OR  atlas*  OR  amateur  OR  {citizen researcher}  OR  {individual citizen scientist}  OR  collaborator  OR  {lay knowledge holder} )  AND  TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY ({North America})  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer )") #260

North_America <- scopus_search(query="TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY ({North America})  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer)") #17934


Europe_cs <- scopus_search(query="TITLE-ABS-KEY ( {community based participatory research}  OR  {community-based participatory research}  OR  {public participation}  OR  hunter  OR  naturalist  OR  atlas*  OR  amateur  OR  {citizen researcher}  OR  {individual citizen scientist}  OR  collaborator  OR  {lay knowledge holder} )  AND  TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY (Europ*)  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer )")#678

Europe <- scopus_search(query="TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY (Europ*)  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer)")#44157


Australia_cs <- scopus_search(query="TITLE-ABS-KEY ( {community based participatory research}  OR  {community-based participatory research}  OR  {public participation}  OR  hunter  OR  naturalist  OR  atlas*  OR  amateur  OR  {citizen researcher}  OR  {individual citizen scientist}  OR  collaborator  OR  {lay knowledge holder} )  AND  TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY (Australia* OR oceania)  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer )")#301

Australia <- scopus_search(query="TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY (Australia* OR oceania)  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer)")#19853


Africa_cs <- scopus_search(query="TITLE-ABS-KEY ( {community based participatory research}  OR  {community-based participatory research}  OR  {public participation}  OR  hunter  OR  naturalist  OR  atlas*  OR  amateur  OR  {citizen researcher}  OR  {individual citizen scientist}  OR  collaborator  OR  {lay knowledge holder} )  AND  TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY (Africa*)  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer )")#502

Africa <- scopus_search(query="TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY (Africa*)  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer)")#25731



South_America_cs <- scopus_search(query="TITLE-ABS-KEY ( {community based participatory research}  OR  {community-based participatory research}  OR  {public participation}  OR  hunter  OR  naturalist  OR  atlas*  OR  amateur  OR  {citizen researcher}  OR  {individual citizen scientist}  OR  collaborator  OR  {lay knowledge holder} )  AND  TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY ({South America})  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer )")#61

South_America <- scopus_search(query="TITLE-ABS-KEY (ecolog*  OR  biodivers*  OR  conservation)  AND  TITLE-ABS-KEY ( monitor*  OR  manage*  OR  survey  OR  conserv* )  AND  TITLE-ABS-KEY ({South America})  AND NOT  TITLE-ABS-KEY (marine  OR  restoration  OR  astron*  OR  medic*  OR  cancer)")#6204


geog_comp <- data.frame(continent = c("North America","Europe","Australia","Africa","South America"),
                        CS = c(260,678,301,502,61),
                        all_lit = c(17934,44157,19853,25731,6204))


geog_comp$proportion_lit <- geog_comp$all_lit/sum(geog_comp$all_lit)

res3 <- chisq.test(x = geog_comp$CS, p = geog_comp$proportion_lit, correct = F)

res3$expected

res3_summary <- data.frame("continent" = geog_comp$continent,
                           "observed" = res3$observed,
                           "expected" = res3$expected)

#taxon_comp$difference <- taxon_comp$proportion - taxon_comp$proportion_lit

geog_comp_plot <- ggplot(res3_summary, aes(y=observed, x=expected, label = continent))+
  geom_point()+
  #geom_text(hjust=0, vjust=-0.5)+
  geom_label_repel(size = 7,aes(size = NULL), nudge_y = 0.7)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
  theme_classic(base_size = 24, base_family = "sans")+
  labs(x="Expected Number of Articles", y="Observed Number of Articles")

png("figures/fig_xsq3.png", height = 10, width = 10, units="in", res = 300)
geog_comp_plot
dev.off()

geog_comp$CS_prop <- geog_comp$CS/geog_comp$all_lit

# n_cs <- length(citsci_data1$ï..Program.Name)
# 
# geo_data_cs <- citsci_data1 %>%
#   group_by(continent) %>%
#   summarise(count_cs = length(continent),
#             proportion_cs = length(continent)/n_cs)
# 
# 
# n_art <- n_distinct(data_clean1$Article)
# 
# geo_data_lit <- data_clean1 %>%
#   group_by(Continent) %>%
#   summarise(count_lit = n_distinct(Article),
#             proportion = n_distinct(Article)/n_art)
# 
# names(geo_data_lit) <- c("continent","count_lit",  "proportion_lit")
# 
# geo_comp_data <- left_join(geo_data_lit, geo_data_cs, by = "continent")
# 
# geo_comp_data1 <- geo_comp_data %>%
#   drop_na(count_cs)
# 
# geo_comp_data1 <- geo_comp_data1 %>%
#   mutate(proportion_cs = count_cs/sum(count_cs))
# 
# 
# res_geo <- chisq.test(x = geo_comp_data1$count_lit, p = geo_comp_data1$proportion_cs, correct = F)
# 
# res_geo$expected
# 
# res_geo_1 <- data.frame("continent" = geo_comp_data1$continent,
#                     "observed" = res_geo$observed,
#                     "expected" = res_geo$expected)
# 
# geo_comp_plot <- ggplot(res_geo_1, aes(y=observed, x=expected, label = continent))+
#   geom_point()+
#   #geom_text(hjust=0, vjust=-0.5)+
#   geom_label_repel(aes(label = continent, size = NULL), nudge_y = 0.7)+
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed")+
#   theme_classic(base_size = 28, base_family = "serif")+
#   labs(x="Expected Number of Articles", y="Observed Number of Articles")
# 
# geo_comp_plot
# 
# png("fig_xsq_cont.png", height = 9, width = 9, units="in", res = 300)
# geo_comp_plot
# dev.off()




