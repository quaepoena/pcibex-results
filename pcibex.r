# Process the output from the Haskell program.
# Based wholly off of Chris Wetherill's code found here:
# https://datascienceplus.com/building-barplots-with-error-bars/

require("ggplot2")
require("dplyr")

csv_data <- read.csv("/tmp/pcibex-output.csv")

# target
items <- c(3,5,7,9,11,13)
# control
#items <- c(4,6,8,10,12,14)

data <- csv_data %>% filter(item %in% items)

data <- aggregate(data$value, by = list(data$item),
                  FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x)))
data <- do.call(data.frame, data)
data$se <- data$x.sd / sqrt(data$x.n)
colnames(data) <- c("item", "mean", "sd", "n", "se")

data$items <- c("/01/", "/03/", "/05/", "/07/", "/09/", "/11/")
#data$items <- c("/02/", "/04/", "/06/", "/08/", "/10/", "/12/")

dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = mean + se, ymin = mean - se)

p <- ggplot(data = data, aes(x = items, y = mean, fill = items))

p + geom_bar(stat = "identity", position = dodge) +
    geom_errorbar(limits, position = dodge, width = 0.25) +
    theme(axis.ticks.x=element_blank())
