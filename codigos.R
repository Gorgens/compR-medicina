
# Capítulo 3

tdic = read.csv("tdics2r.csv", sep = ";", dec=",")
dim(tdic)

dds = read.csv("ddsr.csv", sep = ";", dec=",")
dim(dds)


# Capítulo 4

dds$diagnostico = as.Date(dds$diagnostico, format="%d/%m/%Y")
dds$diagnosticoAno = as.integer(format(dds$diagnostico, format="%Y"))
hist(dds$diagnosticoAno)

boxplot(dds$consultas_ano ~ dds$ideal)

plot(tdic$tdic_pessoal, tdic$tdic_docencia)

# Capítulo 5

shapiro.test(dds$idade_dx)

hist(dds$idade_dx)

summary(dds$idade_dx)

install.packages("epiR")
require(epiR)

table(dds$cor)

pos = c(2, 42, 168, 27)
pop = 239
dat = as.matrix(cbind(pos, pop))
epi.conf(dat, ctype = "prevalence", method = "exact", N = 1000,  design = 1, conf.level = 0.95) * 100

# Capítulo 6

a = table(dds$ideal, dds$psicosocial)
chisq.test(a)

dds$ideal = as.factor(dds$ideal)
levels(dds$ideal) = c(0,1)

logitpsicosocial = glm(ideal~ factor(psicosocial), data = dds, family=binomial)
summary(logitpsicosocial)

logitpsicosocial.or = exp(cbind(OR = coef(logitpsicosocial), confint(logitpsicosocial)))
round(logitpsicosocial.or, digits=4)

t.test(dds$idade ~ dds$ideal)

wilcox.test(dds$evolucao ~ dds$ideal)

kruskal.test(tdic$tdic_docencia ~ tdic$regime)
oneway.test(tdic$tdic_docencia ~ tdic$regime)

kruskal.test(tdic$tdic_docencia ~ tdic$motivacao)

install.packages("dunn.test")
require(dunn.test)
dunn.test(tdic$tdic_docencia, tdic$motivacao)

minas = read.csv("minas.csv", sep=";", dec=",")
cor.test(minas$idh, minas$foraIdeal, method = "spearman")

install.packages("quantreg")
require(quantreg)
fit = rq(tdic_docencia ~ tdic_pessoal, data=tdic)
summary(fit)

install.packages("rcompanion")
require(rcompanion)
nagelkerke(fit)

ddsmenos3a = subset(dds, menos3a == 's')
ddsmais3a = subset(dds, menos3a == 'n')
ks.test(ddsmenos3a$idade_dx, ddsmais3a$idade_dx, alternative="two.sided")

# Capítulo 7

modelTdic = rq(tdic_docencia~tdic_pessoal + motivacao + favorecimento, data=tdic)
summary(modelTdic)
nagelkerke(modelTdic)

# Capítulo 8

multiv = read.csv('multivariada.csv', sep=';', dec=',')

multiv$ideal = as.factor(multiv$ideal)
multiv$menos3a = as.factor(multiv$menos3a)
multiv$psicosocial = as.factor(multiv$psicosocial)
multiv$ocupacao = as.factor(multiv$ocupacao)
multiv$has = as.factor(multiv$has)
multiv$cd4_abaixo350 = as.factor(multiv$cd4_abaixo350)
multiv$leish = as.factor(multiv$leish)

require(party)
        
escore_controle = ctree(ideal ~ psicosocial + ocupacao + has + cd4_abaixo350 + leish + idhm + idhmr +idhml, multiv)

plot(escore_controle)
