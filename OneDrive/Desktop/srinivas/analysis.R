read.csv("data.csv")
d<-read_csv("data.csv")
x<-d$Area
y<-d$Production

#shapiro-wilk Normality Test
shapiro.test(d$Area)
shapiro.test(d$Production)


#correlation test for assymetric data using kendall and spearman rank correlation methods
kend_cor<-cor.test(d$Area,d$Production,method = "kendall")
spear_cor<-cor.test(d$Area,d$Production,method = "spearman")

kend_cor
kend_cor$estimate
kend_cor$p.value

spear_cor
spear_cor$estimate
spear_cor$p.value
