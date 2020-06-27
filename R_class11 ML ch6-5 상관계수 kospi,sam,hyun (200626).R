k_index <- read.csv("K_index.csv", header=T,  stringsAsFactors=F)
s_stock <- read.csv("S_stock.csv", header=T, stringsAsFactors=F)
h_stock <- read.csv("H_stock.csv", header=T,  stringsAsFactors=F)

all_data <- merge(merge(k_index,s_stock), h_stock)
all_data
head(all_data)
attach(all_data)

#plot(k_rate, s_rate, col="blue")
plot(s_rate, k_rate, col="blue") # s to kos
model_s <- lm( k_rate ~ s_rate, data=all_data) # s affect to kos
abline( model_s,  col="red") 

#plot(k_rate, h_rate, col="blue")
plot(h_rate, k_rate, col="blue") # h to kos
model_h <- lm( k_rate ~ h_rate, data=all_data) # h aff to kos
abline( model_h,  col="red") 


graphics.off()
par(mfrow=c(1,2), new=T)
par(mar=c(2,2,2,2) )

plot(k_rate, s_rate, col="blue")
model_s <- lm( s_rate ~ k_rate, data=all_data)
abline( model_s,  col="red") 

plot(k_rate, h_rate, col="blue")
model_h <- lm( h_rate ~ k_rate, data=all_data)
abline( model_h,  col="red") 
