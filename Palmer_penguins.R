if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('palmerpenguins')) install.packages('palmerpenguins'); library('palmerpenguins')
if (!require('knitr')) install.packages('knitr'); library('knitr')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('tidyr')) install.packages('tidyr'); library('tidyr')

inhabited_islands <- penguins %>% group_by(species) %>% summarize(unique(island))
inhabited_islands_df <- data.frame(inhabited_islands)
colnames(inhabited_islands_df) <- c("Species", "Island of Residence")
kable(inhabited_islands_df)

penguins %>% drop_na() %>% ggplot() + 
  geom_jitter(mapping=aes(y=body_mass_g, x=flipper_length_mm, color=species)) + 
  xlab("Length of flippers") + ylab("Body mass") + labs(color="Species") + 
  annotate(geom = "curve", x = 215, y = 3500, xend = 222, yend = 4500, 
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) + 
  annotate(geom= "text", x=215, y=3250, label="The biggest specie")

messages_df <- data.frame(label = c("190 mm", "196 mm", "217 mm"),
                          species = c("Adelie", "Chinstrap", "Gentoo"),
                          xcord = c(210, 215, 197),
                          ycord= c(12.5, 12.5, 12.5)
)
flippers_mean <- penguins %>% drop_na() %>% group_by(species) %>% summarize(mean_of_flippers = mean(flipper_length_mm))

penguins %>% drop_na() %>% ggplot() + geom_bar(mapping=aes(x=flipper_length_mm, fill=species)) + facet_wrap(~species) + 
  geom_vline(data=flippers_mean, mapping=aes(xintercept=mean_of_flippers), color = "black", linetype="dashed") +
  labs(y="Number of Penguins", x="Length of the flippers (mm)", fill="Species") + 
  geom_text(data= messages_df, mapping=aes(x = xcord, y = ycord, label = label, fontface="italic"), color="black")

penguins_mean_weight_df <- data.frame(species = c("Adelie", "Adelie", "Chinstrap", "Chinstrap", "Gentoo", "Gentoo"),
                                      sex = c("female", "male", "female", "male", "female", "male"),
                                      mean_weight_label = c("3369 g", "4043 g", "3527 g", "3939 g", "4680 g", "5485 g"),
                                      xcord = c(4000, 4500, 4000, 4500, 5000, 5000),
                                      ycord = c(5, 5, 5, 5, 5, 5),
                                      adjustment= c(0, 0, 0, 0, 0, 1 )
)

penguins_mean_weight_line <- penguins %>% drop_na() %>% group_by(species, sex) %>% summarize(mean_weight = mean(body_mass_g))

penguins %>%  drop_na() %>% ggplot() + geom_bar(mapping=aes(x=body_mass_g, fill=species)) + 
  facet_wrap(vars(species, sex)) + labs(y="Number of penguins", x="Body mass (g)", fill="Species") + 
  geom_vline(data=penguins_mean_weight_line, mapping=aes(xintercept=mean_weight), color="black", linetype="dashed") + 
  geom_text(data=penguins_mean_weight_df, mapping=aes(x = xcord, y= ycord, label = mean_weight_label, hjust = adjustment), color="black", fontface="italic")