#Ref : http://rstudio.github.io/dygraphs/

install.packages("dygraphs")
library(dygraphs)

lungDeaths <- cbind(mdeaths, fdeaths)
dygraph(lungDeaths)

###

dygraph(lungDeaths) %>% dyRangeSelector()

###

dygraph(lungDeaths) %>%
  dySeries("mdeaths", label = "Male") %>%
  dySeries("fdeaths", label = "Female") %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)

###

hw <- HoltWinters(ldeaths)
predicted <- predict(hw, n.ahead = 72, prediction.interval = TRUE)

dygraph(predicted, main = "Predicted Lung Deaths (UK)") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))


#Decompose data
ld = decompose(ldeaths)
plot(ld)

