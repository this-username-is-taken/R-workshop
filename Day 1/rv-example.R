source("rv.r")

dice <- rv(1:6)
plot(dice)
mean(dice)

P(dice > 3)
plot(dice)
plot(dice + dice)
plot(dice + dice + dice)
plot(dice + dice + dice + dice)
plot(dice + dice + dice + dice + dice)


# Slide ------------------------------------------------------------------

x <- rv(c(-1, 0, 1, 2, 3), c(0.2, 0.1, .3, .1, .3))
mean(x)

png("rv-plot.png", width = 400, height = 400)
par(bg = NA, mar = c(2,2,0,0))
plot(x)
dev.off()
