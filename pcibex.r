# Process the output from the Haskell program.
# Based largely off of Chris Wetherill's tutorial:
# https://datascienceplus.com/building-barplots-with-error-bars/

require("ggplot2")
require("dplyr")

csv_data <- read.csv("pcibex-output.csv")

data <- aggregate(csv_data$value, by = list(csv_data$item),
                  FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x)))
data <- do.call(data.frame, data)
data$se <- data$x.sd / sqrt(data$x.n)
colnames(data) <- c("item", "mean", "sd", "n", "se")

target_items <- c(3,5,7,9,11,13)
target_data <- data %>% filter(item %in% target_items)
target_data$items <- c("/01/", "/03/", "/05/", "/07/", "/09/", "/11/")

control_items <- c(4,6,8,10,12,14)
control_data <- data %>% filter(item %in% control_items)
control_data$items <- c("/02/", "/04/", "/06/", "/08/", "/10/", "/12/")

dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = mean + se, ymin = mean - se)

p_target <- ggplot(data = target_data, aes(x = items, y = mean, fill = items))

p_target + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  theme(axis.ticks.x=element_blank())

p_control <- ggplot(data = control_data, aes(x = items, y = mean, fill = items))

p_control + geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.25) +
  theme(axis.ticks.x=element_blank())

select(data, item, mean)
