v <- read.csv("csvs/model_performance.csv")
v$irate <- v$rate
v$rate <- factor(v$irate)
diseases <- unique(v$disease)


print(kruskal.test(F1 ~ rate, data=v))
print(kruskal.test(F1 ~ model, data=v))
print(kruskal.test(F1 ~ disease, data=v))

kts <- sapply(diseases, function(disease) {
    kruskal.test(F1 ~ rate, data=v[v$disease==disease,])$p.value
})
print("which diseases it's not a factor")
print(diseases[kts >= 0.05])
print("which diseases it is a factor")
print(diseases[kts < 0.05])

print("Afib rate compare")
afib = v[v$disease=="atrial fibrillation",]
print(pairwise.wilcox.test(afib$F1,afib$rate))

print("Sinus Rhythm Compare")
sr = v[v$disease=="sinus rhythm",]
print(pairwise.wilcox.test(sr$F1,sr$rate))

wilcox50500 <- sapply(diseases, function(disease) {
    x <- v[v$disease==disease,]
    wilcox.test(x$F1[x$rate=="50"],x$F1[x$rate=="500"])$p.value
})

print(wilcox50500)
print("which diseases is 50 and 500 not significant? As in 50hz don't hurt.")
print(diseases[wilcox50500 >= 0.05])
print("which diseases is 50 and 500 different?")
print(diseases[wilcox50500 < 0.05])

# anovas kind of violate expectations

two.way <- aov(F1 ~ model + rate + disease, data=v)
print(two.way)
print(summary(two.way))

sapply(diseases, function(disease) {
    print(as.character(disease))
    print(summary( aov(F1 ~ model + rate, data=v[v$disease == disease,])))
})
