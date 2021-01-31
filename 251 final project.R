setwd("/Users/mimi/Desktop")
mostfreqdoc = read.csv("mostfreq1000docword.csv")
mostfreq = read.csv("mostfreq1000word.csv")
speeches = read.csv("speeches.csv")
winners = read.csv("winners.csv")
deceptionworddoc = read.csv("deceptiondocword.csv")
deceptionword = read.csv("deceptionword.csv")

cleanmostfreq <- data.frame(mostfreqdoc)
cleanmostfreq <- data.frame("Speeches" = speeches, cleanmostfreq)
cleanmostfreq <- data.frame(cleanmostfreq, "win/loss" = winners)
cleanmostfreq <- rbind(NA, cleanmostfreq)

x <- vector(mode="character", length=1002)

#myData = as.data.frame(matrix(numeric(),nrow = 0, ncol = length(x)))
x = colnames(cleanmostfreq) 
cleanmostfreq[1,] <- x 

#names(mostfreq) <- NULL
mostfreq <- rbind(mostfreq, "win/loss")
mostfreq <- rbind(NA, mostfreq)
mostfreq <- rbind(NA, mostfreq)
mostfreq[2,] <- colnames(mostfreq)
mostfreq[1,] <- "Speeches"
names(mostfreq) <- NULL
transposemostfreq = t(mostfreq)

colnames(cleanmostfreq) <- transposemostfreq
head(cleanmostfreq)
cleanmostfreq[1,]<-sub(".", "", cleanmostfreq[1,])

cleandeception <- data.frame(deceptionworddoc)
cleandeception <- data.frame("Speeches" = speeches, cleandeception)
cleandeception <- data.frame(cleandeception, "win/loss" = winners)
cleandeception <- rbind(NA, cleandeception)

y <- vector(mode = "character", length = 78)
y = colnames(cleandeception)
cleandeception[1,] <- y

deceptionword <- rbind(deceptionword, "win/loss")
deceptionword <- rbind(NA, NA, deceptionword)
deceptionword[1,] <- "Speeches"
deceptionword[2,] <- colnames(deceptionword)
names(deceptionword) <- NULL
transposedeceptionword = t(deceptionword)

colnames(cleandeception) <- transposedeceptionword
cleandeception[1,] <- sub(".", "", cleandeception[1,])
write.csv(cleanmostfreq, "cleanmostfreq.csv")

neatdeception = read.csv("deceptiondocwordNeat.csv")
deceptionwinner = subset(neatdeception, win.loss == 1)
deceptionloser = subset(neatdeception, win.loss == 0)

deceptionwinner$sum <- rowSums(deceptionwinner[,2:77])
deceptionloser$sum <- rowSums(deceptionloser[,2:77])

totalwinnerdeceptionword <- deceptionwinner$sum
totalwinnerdeceptionword
sumwinnerdeceptionword = sum(totalwinnerdeceptionword)
sumwinnerdeceptionword
meanwinnerdeceptionword = sumwinnerdeceptionword/nrow(deceptionwinner)
meanwinnerdeceptionword

totalloserdeceptionword <- deceptionloser$sum
sumloserdeceptionword = sum(totalloserdeceptionword)
meanloserdeceptionword = sumloserdeceptionword/nrow(deceptionloser)
meanloserdeceptionword

z <- vector(mode = "numeric", length = 78)
z <- colSums(deceptionloser[,2:77])
z
w <- vector(model = "numeric", length = 78)
w <- colSums(deceptionwinner[,2:77])
w
totaldeceptionuse <- z+w
totaldeceptionuse

counts <- table(w/length(w), z/length(z))

plot(totaldeceptionuse, z, col = c("light blue"),  main = "Average Deception Words Usage Winners vs. Losers", 
     xlab = "Words Appear(times)", ylab = "Average Words Uage(times)", pch = 19, xlim = c(0,1000), ylim = c(0,500))
points(totaldeceptionuse, w, col=c("red"), pch = 19)

totaldeceptionuse
length(w)
length(z)

posword <- data.frame(cleanmostfreq)

