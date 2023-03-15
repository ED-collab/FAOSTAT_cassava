library(ggplot2)
library(extrafont)
loadfonts(device = "win")

data<-read.csv("D:/Dropbox/Business case analysis/KU_TTDI/Stems multiplied and disseminated by TTDI.csv")

Try<-data %>% pivot_longer(!Year, names_to = "Variety", values_to = "Number")
Try$Variety <-
  factor(
    Try$Variety,
    levels = c(
      "KU50",
      "R5",
      "R72",
      "HB60",
      "R7",
      "HB80",
      "Waxy",
      "HB90",
      "Unknown"
    )
  )

ggplot(Try, aes(fill=Variety, y=Number/1000000, x=Year)) + 
  geom_vline(xintercept = c(1992, 2003, 2008, 2017, 1994, 2005, 1999, 2013), linetype="dotted", size = 0.3) +
  geom_text(aes(x=1992, label="\nKU50", y=10), colour="black", angle=90, check_overlap = TRUE) +
  geom_text(aes(x=2003, label="\nHB60", y=10), colour="black", angle=90, check_overlap = TRUE) +
  geom_text(aes(x=2008, label="\nHB80", y=10), colour="black", angle=90, check_overlap = TRUE) +
  geom_text(aes(x=2017, label="\nHB90", y=10), colour="black", angle=90, check_overlap = TRUE) +
  geom_text(aes(x=1994, label="\nR5", y=10), colour="black", angle=90, check_overlap = TRUE) +
  geom_text(aes(x=2005, label="\nR7", y=10), colour="black", angle=90, check_overlap = TRUE) +
  geom_text(aes(x=1999, label="\nR72", y=10), colour="black", angle=90, check_overlap = TRUE) +
  geom_text(aes(x=2013, label="\nWaxy (1st gen)", y=9), colour="black", angle=90, check_overlap = TRUE) +
  geom_bar(position="stack", stat="identity") +
  theme_pubr() +
  theme(legend.position = 'right') +
  scale_fill_grey() + 
  ylab("Number of stems (millions)") + 
  scale_y_continuous(breaks=seq(0,11,2)) + 
  scale_x_continuous(breaks=seq(1990,2020,5))
