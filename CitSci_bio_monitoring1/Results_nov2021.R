# Results Nov 2021

#figures and results
library(tidyverse)
library(ggsci)
library(ggpubr)
library(GGally)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(scales)
library(extrafont)


theme_set(theme_classic(base_size = 10, base_family = "serif"))

load("data/data_clean_nov10.RData")

#### figure 1 ####

mypal2 = pal_npg("nrc", alpha = 1)(2)

notlisted <- data_clean1 %>%
  #filter(is.na(Sp_status))%>%
  filter(Sp_status == "introduced" | 
           Sp_status == "invasive" | 
           Sp_status == "not listed" |
           Sp_status == "biological control agent" |
           Sp_status == "least concern" |
           Sp_status == "indicator" |
           Sp_status == "pest" |
           Sp_status == "non-indigenous" |
           is.na(Sp_status))

not_listed_by_taxa <- notlisted %>%
  group_by(taxa_clean) %>%
  summarise(n_articles = length(unique(Article)),
            n_species = length(unique(species_level))) %>%
  pivot_longer(cols = c(n_articles, n_species),
               names_to = "variable",
               values_to = "value")

not_listed_by_taxa$status <- rep("not listed", length(not_listed_by_taxa$taxa_clean))

listed <- data_clean1 %>%
  filter(!is.na(Sp_status))%>%
  filter(Sp_status != "introduced" & 
           Sp_status != "invasive" & 
           Sp_status!= "not listed" &
           Sp_status != "biological control agent" &
           Sp_status != "least concern" &
           Sp_status != "indicator" &
           Sp_status != "pest" &
           Sp_status != "non-indigenous")

listed_by_taxa <- listed %>%
  group_by(taxa_clean)%>%
  summarise(n_articles = n_distinct(Article),
            n_species = n_distinct(species_level))%>%
  pivot_longer(cols = c(n_articles, n_species),
               names_to = "variable",
               values_to = "value")

listed_by_taxa$status <- rep("listed", length(listed_by_taxa$taxa_clean))

fig2 <- rbind(not_listed_by_taxa,listed_by_taxa)
fig2$status <- factor(fig2$status, levels= c("not listed","listed"))
fig2a_data <- fig2 %>%
  filter(variable == "n_articles") # this is supplemental
fig2b_data <- fig2 %>%
  filter(variable == "n_species") #this is figure 1

#n_articles
fig2a <- ggplot(fig2a_data, aes(x=reorder(taxa_clean, - value), y = value, fill = status))+
  geom_bar(position="stack", stat="identity")+
  theme_classic(base_size = 24, base_family = "serif")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  labs(x="Taxon", y="Number of Articles")+
  #ggtitle("No. Articles")+
  scale_fill_manual(limits = c("not listed","listed"), values=mypal2, name = "Status")
fig2a

png("figureS1.png", height = 10, width = 10, units="in", res = 300)
fig2a
dev.off()

#n_species
fig2b <- ggplot(fig2b_data, aes(x=reorder(taxa_clean, - value), y = value, fill = status))+
  geom_bar(position="stack", stat="identity")+
  theme_classic(base_size = 24, base_family = "serif")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, family="serif"))+
  labs(x="Taxon", y="Number of Species")+
  #ggtitle("No. species")+
  scale_fill_manual(limits = c("not listed","listed"), values=mypal2, name = "Status")
fig2b

#plot_list2 <- list(fig2a,fig2b)

#figure2 <- ggarrange(plotlist = plot_list2,
 #                    common.legend = T,
 #                    ncol = 1,
 #                    nrow = 2,
 #                    #label.x = "Taxon",
 #                    #label.y = "Count",
 #                    legend = "right",
 #                    align = "hv")%>%
 # annotate_figure(figure, left = textGrob("Count", rot = 90, vjust = 1, gp = gpar(cex = 1.3)), bottom = textGrob("Taxon", gp = gpar(cex = 1.3)))

#png("fig2.png", height = 11.5, width = 10, units="in", res = 300)
#figure2
#dev.off()

#figure 1 now, and only using 1b
png("figure1.png", height = 10, width = 10, units="in", res = 300)
fig2b
dev.off()


#### Figure 2 ####

by_country <- data_clean1 %>%
  group_by(Continent, Country, taxa_clean) %>%
  summarise(n_articles = length(unique(Article)),
            n_species = length(unique(species_level))) %>%
  filter(Continent != "Antarctica" &
           Continent != "NA")

by_country$n_species <- as.numeric(by_country$n_species)
by_country %>%
  mutate(n_species = fct_reorder(Country, n_articles, .fun='length'))

by_country$Country <- by_country$Country %>%
  str_to_title()

u_continents <- unique(by_country$Continent)

mypal = pal_npg("nrc", alpha = 1)(8)

plot_list <- list()

for(a in 1:length(u_continents)){
  
  cont <- u_continents[a]
  
  plot <- by_country %>%
    filter(Continent == cont)%>%
    ggplot(aes(fill = taxa_clean, x=reorder(Country, n_articles), y=n_articles, by = Country))+
    #scale_fill_npg()+
    ggtitle(cont)+
    scale_fill_manual(limits = c("all","bird","invertebrate","mammal","plant","amphibian","reptile","fungi"), values=mypal, name = "Taxon", element_text(size = 16, family = "serif"))  + #
    geom_bar(position="stack", stat="identity") +
     theme_classic(base_family = "serif")+
    ylim(0,90)+
    theme(axis.text.x = element_text(vjust = 0.5, hjust=1, family = "serif"),
          # legend.position = "none"
          legend.title=element_text(size=14, family = "serif"),#)+
          legend.text=element_text(size=14, family = "serif"))+
    labs(x="", y="")+
    coord_flip()
  
  plot_list[[a]] <- plot
  
}


figure <- ggarrange(plotlist = plot_list,
                    common.legend = T,
                    ncol = 2,
                    nrow = 3,
                    label.x = "Country",
                    label.y = "Number of Articles",
                    legend = "bottom",
                    align = "hv")#%>%

png("figure2.png", height = 12, width = 8, units="in", res = 300)
figure
dev.off()


#### Figure S2 ####
figS2 <- ggplot(mod_data, aes(log(gdp), (n_studies))) +
  geom_point()+
  geom_smooth(method = "lm")+
  # geom_text(aes(label=Country))+
  labs(y="No. Articles",
       x="log-GDP per capita")+
  theme_classic(base_size = 24)+

png("figureS2.png",height = 10, width = 10, units="in", res = 300)
figS2
dev.off()


#### Figure 4 ####

library(networkD3)

#mypal3 = pal_npg("nrc", alpha = 1)(13)
#https://www.r-graph-gallery.com/322-custom-colours-in-sankey-diagram.html

#links <- data.frame(goals_threats_metrics$Article, goals_threats_metrics$threat, goals_threats_metrics$response)
links <- data.frame(data_clean1$Article, data_clean1$threat1, data_clean1$response1)
colnames(links) <- c("article", "predictor", "response")

links$predictor <- links$predictor %>%
  str_to_title()
links$response <- links$response %>%
  str_to_title()

links1 <- unique(links)

nodes <- data.frame(
  name=c(as.character(links1$predictor), 
         as.character(links1$response)) %>% unique()
)

links1$IDsource <- match(links1$predictor, nodes$name)-1 
links1$IDtarget <- match(links1$response, nodes$name)-1
links1$value <- rep(1, length(links1$article))

#save(links1, file="data/outputs/sankey_data.RData")

library("RColorBrewer")
#brewer.pal(n = 14, name = "RdBu")
nb.cols <- 14
mycolors <- colorRampPalette(brewer.pal(8, "RdBu"))(nb.cols)
display.brewer.pal(mycolors)

# Make the Network
p <- sankeyNetwork(Links = links1, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", 
                   NodeID = "name",
                   fontSize = 18,
                   sinksRight=F,
                   colourScale = JS(
                     'd3.scaleOrdinal()
                     .domain(["Habitat Loss","Climate Change","No Threat","Invasives","Infectious Diseases","Biocontaminants","Harvesting""Abundance And Trends","Distribution And Range Shifts","No Response","Phenology","Life-History Evolution","Richness, Diversity, Community Composition","Genetics"])
                     .range(["#B2182B","#C53E3D","#D86551","#E88A6D","#F5AD8C","#FACAB1","#F2DDD0","#DBE2E6","#BDDBEA","#9BC9E0","#73B1D3","#4996C5","#337EB8", "#2166AC"])'))
p

onRender(
  p,
  '
  function(el, x) {
    d3.selectAll(".node text").attr("text-anchor", "send").attr("x", 20);
  }
  '
)

#png("figure3.png", height = 11.5, width = 8, units="in", res = 300)
#p
#dev.off()



library(networkD3)

load("data/outputs/sankey_data.RData")

nodes <- data.frame(
  name=c(as.character(links1$predictor), 
         as.character(links1$response)) %>% unique()
)

p <- sankeyNetwork(Links = links1, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", 
                   NodeID = "name", 
                   fontSize = 16,
                   sinksRight=FALSE)
p


