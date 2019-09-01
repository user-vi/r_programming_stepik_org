install.packages("xtable")
library(xtable)

install.packages("stargazer")
library(stargazer)

fit1 <- lm(cyl~disp, mtcars)
fit1_table <- xtable(fit1)

print(fit1_table, type = "html", file = "name1.html")

stargazer(fit1, type = "html", 
          dep.var.labels = "cyl",
          covariate.labels = "disp", out = "name2.html")
