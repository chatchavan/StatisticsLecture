
# one sample
wpm <- rnorm(20, mean = 50, sd = 8)
errRate <- rnorm(20, mean = 5, sd = 5)
tct <- rnorm(20, mean = 80, sd = 20)
errRate[errRate < 0] <- 0
df <- data.frame(WPM = wpm, ErrorRate = errRate, TCT = tct)
write.csv(df, "data/typing-OneSample.csv", row.names = F, na = "")


