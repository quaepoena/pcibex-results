# Process the output from the Haskell program.
# Based wholly off of Chris Wetherill's code found here:
# https://datascienceplus.com/building-barplots-with-error-bars/

require("ggplot2")

csv_data <- read.csv("/tmp/pcibex-output.csv")

data <- aggregate(csv_data$value, by = list(csv_data$item),
                  FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x)))
data <- do.call(data.frame, data)
data$se <- data$x.sd / sqrt(data$x.n)
colnames(data) <- c("item", "mean", "sd", "n", "se")
data$names <- c("/01/", "/02/", "/03/", "/04/", "/05/", "/06/", "/07/", "/08/",
                "/09/", "/10/", "/11/", "/12/", "/13/", "/14/", "/15/", "/16/",
                "/17/", "/18/", "/19/", "/20/", "/21/", "/22/", "/23/", "/24/")

dodge <- position_dodge(width = 0.9)
limits <- aes(ymax = mean + se, ymin = mean - se)

p <- ggplot(data = data, aes(x = names, y = mean, fill = names))

p + geom_bar(stat = "identity", position = dodge) +
    geom_errorbar(limits, position = dodge, width = 0.25) +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.x=element_blank())