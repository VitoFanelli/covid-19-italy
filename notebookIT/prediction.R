setwd(.rs.getProjectDirectory())
library(tidyverse)

casi <- italia %>% 
  select(data, contagi) %>% 
  mutate(casi = c(229,diff(contagi)))

x = casi$casi
last = x[length(x)]
y = casi$contagi[nrow(casi)]

effetto5 = 0.95
effetto10 = 0.90
effetto20 = 0.80

while(last > 1){
  last = last*effetto5
  y = c(y, last)
}
pred5 = cumsum(y)[-1]

while(last > 1){
  last = last*effetto10
  y = c(y, last)
}
pred10 = cumsum(y)[-1]

while(last > 1){
  last = last*effetto20
  y = c(y, last)
}
pred20 = cumsum(y)[-1]

prediction5 <- data.frame(
  data = seq.Date(max(casi$data)+1, 
                  max(casi$data)+length(pred5), 
                  by = "day"),
  pred = pred5
)

prediction10 <- data.frame(
  data = seq.Date(max(casi$data)+1, 
                  max(casi$data)+length(pred10), 
                  by = "day"),
  pred = pred10
)

prediction20 <- data.frame(
  data = seq.Date(max(casi$data)+1, 
                  max(casi$data)+length(pred20), 
                  by = "day"),
  pred = pred20
)

highchart(type = "stock") %>% 
  hc_add_series(italia, hcaes(x = data, y = contagi),
                name = "Contagi", type = "line",
                color = "black") %>% 
  hc_add_series(prediction5, hcaes(x = data, y = pred),
                name = "Previsione 5%", type = "line",
                color = "red") %>% 
  hc_add_series(prediction10, hcaes(x = data, y = pred),
                name = "Previsione 10%", type = "line",
                color = "blue") %>% 
  hc_add_series(prediction20, hcaes(x = data, y = pred),
                name = "Previsione 20%", type = "line",
                color = "green") %>% 
  hc_legend(enabled = T)

save(prediction5, prediction10, prediction20,
     file = "./notebookIT/predictions.RData")



















