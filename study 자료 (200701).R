obs <- c(19, 41, 40)
null.probs <- c(2/10, 3/10, 5/10)
chisq.test(obs, p=null.probs)

states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit=lm(Murder~Population+Illiteracy+Income+Frost,data=states)
summary(fit)
confint(fit)

fit <- lm(weight~height,data=women)
fit
dev.new()
plot(weight~height,data=women)
abline(fit,col="red")
title(expression(italic(weight==3.45%*%height-87.52)))
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))
