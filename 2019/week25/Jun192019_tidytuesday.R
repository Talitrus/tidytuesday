####################
# Bryan Nguyen
####################
# Data is from https://www.birdscanada.org/
# Cleaned and tidied by @_sharleen_w on Twitter.
############################################
# This code performs an ordination on the Christmas Bird Count (CBC) data from Hamilton, Ontario, Canada.
# Specifically, it uses a Principle Coordinates Analysis on the Bray-Curtis distance matrix of yearly data.
# The resulting plot is colored by year to show a trend through time.
# Some years were missing CBC data and were removed.
#######################################

library(tidyverse)
library(LaCroixColoR)
library(vegan)
library(ggrepel)
library(extrafont)
bird_counts <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")


table <- spread(data = bird_counts %>% select(year, species_latin, how_many_counted_by_hour), key = "year", value = "how_many_counted_by_hour", fill = 0) %>% 
  column_to_rownames(var = "species_latin")
vegan_table <- t(table)

vegan_table <- vegan_table[rowSums(vegan_table) > 0,] # remove birds with no sightings in Hamilton ever.

bc_matrix <- vegdist(as.matrix(vegan_table)) # Calculate (abundance-weighted) Bray-Curtis ecological community distances between years.

PCoA_ord <- cmdscale(bc_matrix, k = 2, eig = TRUE) # Principle Coordinates Analysis
PCoA_df <- tibble(year = as.numeric(rownames(PCoA_ord$points)), x = PCoA_ord$points[,1], y = PCoA_ord$points[,2])
years_to_label <- c(1921, 1939, 2017)
PCoA_df$label <- "" # Years with a blank label are not labelled.
PCoA_df$label[match(years_to_label, PCoA_df$year)] <- years_to_label # Label years to label with the corresponding year information.

PCoA_plot <- ggplot(data = PCoA_df) +
  geom_point(mapping = aes(x = x, y = y, color = year), size = I(5), alpha = 0.70) +
  #viridis::scale_color_viridis() + # sorry, but the LaCroix color palettes are too much fun.
  scale_color_gradientn(colors = lacroix_palette("PeachPear")) +
  geom_label_repel(mapping = aes(x = x, y = y, label = label, color = year), force = 5) +
  xlab(paste0("PCoA axis 1: ", round(PCoA_ord$eig[1] / sum(PCoA_ord$eig) * 100, digits = 1), "%")) +
  ylab(paste0("PCoA axis 2: ", round(PCoA_ord$eig[2] / sum(PCoA_ord$eig) * 100, digits = 1), "%")) +
  ggtitle("Hamilton, Canada CBC bird communities change / time") +
  theme_minimal() +
  theme(
    text = element_text(
      family = "Lato" # This is a freely available Google font that I imported into R with the 'extrafont' library.
        ),
    plot.title = element_text(
      face="bold")
  )
PCoA_plot

ggsave(filename = "PCoA_plot.png", plot = PCoA_plot, height = 4, width = 6)
