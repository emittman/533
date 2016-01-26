exposure <- c('0-8','8-12','12-16','16-20','20-24','24-28','28-32','32-36','36-40','40-44','44+')
mid <- c(4,10,14,18,22,26,30,34,38,42,46)
cracked <- c(0,4,2,7,5,9,9,6,22,21,21)
not_cracked <- c(39,49,31,66,25,30,33,7,12,19,15)
data <- data.frame(exposure=exposure,midpoint = mid, cracked = cracked, not_cracked = not_cracked)
data
library(plyr)
df <- ddply(data,.(exposure,midpoint,cracked,not_cracked), summarise,
            n = cracked+not_cracked,
            p_cracked = cracked/(cracked+not_cracked),
            se = sqrt(p_cracked * (1-p_cracked)/n),
            high = p_cracked + 2*se,
            low = p_cracked - 2*se
)
df
df <- df[c(1,11,2:10),]
df

library(xtable)
print(xtable(df[,c("midpoint","p_cracked","se")], caption = "Tabulated turbine wheel data with estimated proportions."),
      include.rownames = FALSE)

library(ggplot2)
ggplot(data = df, aes(x = midpoint, y = p_cracked, ymin = low, ymax = high)) +
  geom_point() + geom_errorbar(linetype = "dashed")
ggsave("cis.pdf")