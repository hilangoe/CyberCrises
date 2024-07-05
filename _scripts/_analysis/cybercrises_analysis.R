# script for running various analyses for cyber crisis paper
# three main models: 
# M1: severity of cyber incidents to international crises (unit of analysis: cyber incidents)
# M2: onset of cyber incident(s) to international crises (unit of analysis: dyad-year)
# M3: onset of cyber incidents within international crises to escalation of hostilities (unit of analysis: international crisis)

library(tidyverse)
library(ggplot2)
library(sandwich)
library(plm)
library(stargazer)
library(knitr)
library(kableExtra)


# M1 ----------------------------------------------------------------------

# graphs and descriptive stats

sev_bar <- ggplot(df1, aes(x = severity)) +
  geom_bar() +
  labs(y = "Frequency", x = "Severity")

png("_output/_figures/sev_bar.png")
print(sev_bar)
dev.off()

# model
m1 <- glm(crisis ~ severity + cyber_objective + hostlev + previous_sev + v2x_polyarchy1 + v2x_polyarchy2 + terr_dispute + sipri_milex1 + sipri_milex2, data = df1, family = 'binomial')
summary(m1)


# M2 ----------------------------------------------------------------------

# graphing and descriptive stats



# model
m2 <- glm(crisis_onset ~ cyber + hostlev + previous_sev + v2x_polyarchy1 + v2x_polyarchy2 + terr_dispute + sipri_milex1 + sipri_milex2, data = subset(df2, ongoing==0), family = 'binomial')
summary(m2)
coeftest(m2, vcov = vcovHC(m2))

m2_alt <- glm(crisis_onset ~ cyber + hostlev + previous_sev + v2x_polyarchy1 + v2x_polyarchy2 + terr_dispute + sipri_milex1 + sipri_milex2, data = subset(df2, ongoing==0 & reverse==0), family = 'binomial')
summary(m2_alt)
coeftest(m2_alt, vcov = vcovHC(m2_alt))

# doing a RE model to allow for cluster-robust SE
m2_random <- plm(crisis_onset ~ cyber + hostlev + previous_sev + v2x_polyarchy1 + v2x_polyarchy2 + terr_dispute + sipri_milex1 + sipri_milex2, 
                    data = subset(df2, ongoing == 0 & reverse==0), 
                    model = "random",
                    effect = "twoways",
                    index = c("dyadid", "year"),
                    na.action = na.exclude)

m2_cluster_robust_se <- vcovHC(m2_random, cluster = "group", type = "HC0")

summary(m2_random)
m2_cluster_test <- coeftest(m2_random, vcov = m2_cluster_robust_se)
m2_cluster_test
# significantly weaker results

# # Extract coefficient estimates and standard errors
# coef_est <- m2_cluster_test[, 1]
# std_err <- m2_cluster_test[, 2]
# 
# # Combine coefficient estimates and standard errors
# m2_cluster_output <- cbind(coef_est, std_err)
# 
# # Add column names
# colnames(m2_cluster_output) <- c("Estimate", "Std. Error")
# 
# # Display results with p-values using stargazer
# stargazer(m2_random, type = "text", se = m2_cluster_output)

# M3

m3 <- glm(disp_out1 ~ cyber + hostlev + previous_sev + v2x_polyarchy1 + v2x_polyarchy2 + terr_dispute + sipri_milex1 + sipri_milex2, family = binomial, data = df3)
summary(m3)
# some separation problems here, with nearly perfectly predicted covariates
# these standard errors are not trust-worthy
# note: positive coefficient for cyber means reducing tension post-crisis

m3_sparse <- glm(disp_out1 ~ cyber + hostlev + previous_sev + v2x_polyarchy1 + v2x_polyarchy2 + terr_dispute, family = binomial, data = df3)
summary(m3_sparse)

# chi-square
chisq.test(df3$disp_out1, df3$cyber, correct=FALSE)

# crosstabs
m3_cross <- table(df3$disp_out1, df3$cyber)
m3_cross
class(m3_cross)

# all together

stargazer(m1, type = "html", out = "_output/_tables/m1.html")

stargazer(m2_alt, m2_random, type = "html", out = "_output/_tables/m2.html")


m3_cross_df <- data.frame(
  Row = row.names(m3_cross),
  Outcome1 = c(m3_cross[1, 1], m3_cross[2, 1]),
  Outcome2 = c(m3_cross[1, 2], m3_cross[2, 2])
)

table_html <- kable(m3_cross_df, format = "html", caption = "Cross-tabulation of dispute outcomes and cyber incidents.")
writeLines(capture.output(table_html), "_output/_tables/m3_cross.html")


#stargazer(m3_cross, type = "html", out = "_output/_tables/m3_cross.html")

stargazer(m3_sparse, type = "html", out = "_output/_tables/m3.html")
