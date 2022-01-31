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
mypal3 <- c("#4DBBD5FF","#E64B35FF")

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

fig1_data <- rbind(not_listed_by_taxa,listed_by_taxa)
fig1_data$status <- factor(fig1_data$status, levels= c("not listed","listed"))
figS1_data <- fig1_data %>%
  filter(variable == "n_articles") # this is supplemental
figS1_data$taxa_clean[figS1_data$taxa_clean == "all"] <- "multi-taxa"
fig1_data$status <- factor(fig1_data$status, levels= c("not listed","listed"))
fig1m_data$taxa_clean%>%
  filter(variable == "n_species",
         taxa_clean != "all") #this is figure 1

#n_articles
figS1 <- ggplot(figS1_data, aes(x=reorder(taxa_clean, - value), y = value, fill = status))+
  geom_bar(position="stack", stat="identity")+
  theme_classic(base_size = 28, base_family = "serif")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  labs(x="Taxon", y="Number of Articles")+
  #ggtitle("No. Articles")+
  scale_fill_manual(limits = c("not listed","listed"), labels = c("not threatened","threatened"), values=mypal3, name = "Status")
figS1

png("figures/figureS1.png", height = 10, width = 10, units="in", res = 300)
figS1
dev.off()

#n_species
fig1m <- ggplot(fig1m_data, aes(x=reorder(taxa_clean, - value), y = value, fill = status))+
  geom_bar(position="stack", stat="identity")+
  theme_classic(base_size = 28, base_family = "serif")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, family="serif"))+
  labs(x="Taxon", y="Number of Species")+
  #ggtitle("No. species")+
  scale_fill_manual(limits = c("not listed","listed"), labels = c("not threatened","threatened"), values=mypal3, name = "Status")
fig1m

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
png("figures/figure1.png", height = 10, width = 10, units="in", res = 300)
fig1m
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
library(tidyverse)
load("data/data_clean_nov10.RData")

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
threat_col <- brewer.pal(n = 7, name = "YlOrRd")
resp_col <- brewer.pal(n = 7, name = "GnBu")
#YlOrRd
#GnBu

#nb.cols <- 14
#mycolors <- colorRampPalette(brewer.pal(8, "RdBu"))(nb.cols)
#display.brewer.pal(threat_col)

nodes$group <- gsub(" ", "-", nodes$name)

nodes$name <- gsub("Abundance And Trends", "Abundance and Trends", nodes$name)
nodes$name <- gsub("Distribution And Range Shifts", "Distribution and Range Shifts", nodes$name)


#https://stackoverflow.com/questions/48459033/sankey-network-manual-colour-change

ColourScale <- 'd3.scaleOrdinal()
            .domain(["Habitat-Loss","Climate-Change","No-Threat","Invasives","Infectious-Diseases","Biocontaminants","Harvesting","Abundance-And-Trends","Distribution-And-Range-Shifts","No-Response","Phenology","Life-History-Evolution","Richness,-Diversity,-Community-Composition","Genetics"])
           .range(["#800026","#fc4e2a","#bd0026","#feb24c","#fed976","#ffffcc","#ffffcc","#293e47", "#1b292f","#87ceeb","#5f90a5","#517c8d","#36525E","#6ca5bc"]);'

p <- sankeyNetwork(Links = links1, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", 
                   NodeID = "name", 
                   fontSize = 16,
                   colourScale = JS(ColourScale),
                   NodeGroup = "group",
                   sinksRight=FALSE)
p

library(htmlwidgets)

#https://stackoverflow.com/questions/61172342/pushing-left-labels-to-the-left-of-the-nodes-in-sankey-diagram

nodes$target <- c(rep(TRUE,7),rep(FALSE,7))

p$x$nodes$target <- nodes$target

q <- onRender(
  p,
  '
  function(el, x) {
    d3.selectAll(".node text")
    .filter(d => d.target)
    .attr("text-anchor", "end")
    .attr("x", -6);
  }
  '
)

#doesnt work 
png("figure3.png", height = 8, width = 11.5, units="in", res = 300)
onRender(
  p,
  '
  function(el, x) {
    d3.selectAll(".node text")
    .filter(d => d.target)
    .attr("text-anchor", "end")
    .attr("x", -6);
  }
  '
)
dev.off()

#also not working
library(webshot)

webshot::install_phantomjs()

saveWidget(q, "temp.html")
webshot("temp.html", "sankey_widget.png")



library(networkD3)

load("data/outputs/sankey_data.RData")

nodes <- data.frame(
  name=c(as.character(links1$predictor), 
         as.character(links1$response)) %>% unique()
)

#nodes$id <- 0:(nrow(nodes) - 1)

nodes$group <- gsub(" ", "-", nodes$name)

ColourScale <- 'd3.scaleOrdinal()
            .domain(["Habitat-Loss","Climate-Change","No-Threat","Invasives","Infectious-Diseases","Biocontaminants","Harvesting","Abundance-And-Trends","Distribution-And-Range-Shifts","No-Response","Phenology","Life-History-Evolution","Richness,-Diversity,-Community-Composition","Genetics"])
           .range(["#800026","#fc4e2a","#bd0026","#feb24c","#fed976","#ffffcc","#ffffcc","#293e47", "#1b292f","#87ceeb","#5f90a5","#517c8d","#36525E","#6ca5bc"]);'

p <- sankeyNetwork(Links = links1, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", 
                   NodeID = "name", 
                   fontSize = 16,
                   colourScale = JS(ColourScale),
                   NodeGroup = "group",
                   sinksRight=FALSE)
p


####  Box figures #### 
# updated: January 31 2022


library(tidyverse)
library(ggrepel)
library(MASS)
library(ggrepel)
library(countrycode)
library(gridExtra)
library(ggsci)
library(ggpubr)

data<-read.csv("~/GitHub/Citizenscience_paper/CitSci_bio_monitoring1/data/data_clean_nov10.csv")
data$Country<-str_to_title(data$Country)
#data$Countrycode<-countrycode(data$Country_caps, origin = "country.name", destination = "ioc")
#data$Countrycode<-ifelse(is.na(data$Countrycode), data$Country_caps, data$Countrycode)

# group data by country
by_country<-data %>%
  group_by(Country) %>% 
  summarise(n_proj = n_distinct(citsci_proj_ID),
            n_studies = n_distinct(row_ID),
            n_Articles = n_distinct(Article))

#kendall correlation (non parametric)
cor.test( ~ n_proj + n_Articles,
          data = by_country,
          method = "kendall",
          continuity = TRUE,
          conf.level = 0.95)

library(mblm)
library(mgcv)
#Kendall-Thein Sen Siegel nonparametric linear regression
model_k<-mblm(n_proj ~ n_Articles,
              data = by_country)
summary(model_k)


# plot function (https://sejohnston.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/)
# visualize residuals (https://drsimonj.svbtle.com/visualising-residuals)



fit<-lm(n_proj~n_Articles, data = by_country)
plot_data<-by_country


plot_data$predicted<-predict(fit)
plot_data$residuals<-residuals(fit)
plot_data$studres<-studres(fit)

# Get outliers (defined as having a studentized residuals >3 or < -3)
studres_labels<-filter(plot_data, studres>3 |studres< (-3))
plot_data<-mutate(plot_data, type = ifelse(studres>3 |studres< (-3), 'Outlier', 'Non-outlier'))

# Get df with points needing labels. 
USA_lab<-dplyr::filter(plot_data,(Country=="United States"))
UK_lab<-dplyr::filter(plot_data, (Country=="United Kingdom")) 
SA_lab<-dplyr::filter(plot_data,(Country=="South Africa"))
Den_lab<-dplyr::filter(plot_data,(Country=="Denmark"))

regression<- ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
  geom_smooth(method = "lm", se = TRUE, color = "black") + #model line with grey se
  
  geom_jitter(aes(color = plot_data$type),
              size = 2,alpha = 0.5) + # Color and size mapped here. Size can be changed to: abs(plot_data$residuals))
  scale_color_manual("Study Location",values = c("Outlier" = "red", "Non-outlier" = "black"))+
  coord_fixed(2)+
  
  xlim(c(-12,85))+
  
  theme_classic(base_family = "serif") + # Add theme for cleaner look
  theme(legend.position = "right",
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))+
  
  
  geom_text(data = USA_lab, aes(label = Country),
            family = "serif",
            size = 3.5,
            hjust =1.05)+
  
  geom_text(data = UK_lab, aes(label = Country),
            family = "serif",
            size = 3.5,
            hjust =1.1)+
  
  geom_text(data = SA_lab, aes(label = Country),
            family = "serif",
            size = 3.5,
            hjust =-.05)+
  
  geom_text(data = Den_lab, aes(label = Country),
            family = "serif",
            size = 3.5,
            vjust =-0.6,
            hjust = .9)+
  
  
  labs(#title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
    #      "Intercept =",signif(fit$coef[[1]],5 ),
    #      " Slope =",signif(fit$coef[[2]], 5),
    #      " P =",signif(summary(fit)$coef[2,4], 5),
    #      "\nCall = ", fit$call, "(", names(fit$model)[1],"~", names(fit$model)[2], ")"),
    x = "Number of articles reviewed",
    y = "Number of unique community science projects")

regression


ggsave(filename = "figures/Box_regression.png",
       width =5, height = 6) 




# format data long version for Denmark and SA 
# identify species of conservation concern.
status_data<-filter(data, (Country == "Denmark")|(Country == "South Africa")) %>%
  mutate(status = ifelse(is.na(Sp_status), "non-threatened", "threatened")) %>% 
  group_by(Country, Taxon, status) %>%
  summarize(article_count = n_distinct(Article),
            project_count = n_distinct(citsci_proj_ID)) 

status_data$Taxon[status_data$Taxon == "all"] <- "multi-taxa"
order_status<-c("threatened", "non-threatened")


# new facet label names for Countries
names_top<-list('Denmark'= "a) Denmark",
                'South Africa'= "b) South Africa")
top_labeller<-function(variable, value){
  return(names_top[value])
}

names_bottom<-list('Denmark'= "c) Denmark",
                   'South Africa'= "d) South Africa")
bottom_labeller<-function(variable, value){
  return(names_bottom[value])
}


a_count<-ggplot(status_data, aes(Taxon, article_count, fill = status))+
  geom_col(position = "stack")+
  theme_classic(base_family = "serif")+
  theme(strip.background = element_blank(),
        text = element_text(size = 12),
        axis.text.x = element_text(size = 10,angle = 45, hjust =1),
        axis.title = element_text(size = 10),
        axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0)),
        panel.spacing = unit(2, "lines"))+
  labs(fill = "species status")+
  xlab("Focal taxa")+
  ylab("Number of articles reviewed")+
  scale_fill_manual(values = c("#4DBBD5FF", "#E64B35FF"))+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,15))+
  facet_grid(.~Country,
             labeller = top_labeller)

p_count<-ggplot(status_data, aes(Taxon, project_count, fill = status))+
  geom_col(position = "stack")+
  theme_classic(base_family = "serif")+
  theme(strip.background = element_blank(),
        text = element_text(size = 12),
        axis.text.x = element_text(size = 10,angle = 45, hjust =1),
        axis.title = element_text(size = 10),
        axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0)),
        panel.spacing = unit(2, "lines"))+
  labs(fill = "species status")+
  xlab("Focal taxa")+
  ylab("Number of unique community science projects")+
  scale_fill_manual(values = c("#4DBBD5FF", "#E64B35FF"))+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,5))+
  facet_grid(.~Country,
             labeller = bottom_labeller)
ggarrange(a_count, p_count, ncol = 1, nrow = 2,
          common.legend = TRUE, legend="right")

ggsave(filename = "figures/Box_bars.png",
       width =5, height = 6,
       dpi=300) 




