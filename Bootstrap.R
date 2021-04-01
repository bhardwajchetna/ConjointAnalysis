library('MASS')

screen75 <- rep(c(1,0,0),8)
screen85 <- rep(c(0,1,0),8)
resolution4k <- rep(c(0,0,0,1,1,1),4)
Sony <- rep(c(1,1,1,1,1,1,0,0,0,0,0,0),2)
Price <- c(rep(0,12), rep(1,12))
medrank <- c(11.5,16,8,22.5,23.5,15,9.5,9,5.5,17.5,21,9.5,13,13,2.5,18.5,17,9.5,6.5,11.5,1,14,15.5,6.5)
data <- data.frame(screen75, screen85, resolution4k, Sony, Price, medrank)
y = as.matrix(data[,6])
x = as.matrix(data[,1:5])
model = lm(y ~ x)
rr = model$residuals
yhat = predict(model)
nn = nrow(data)

bb = 1000
RBS_beta = matrix(NaN, bb, 6)
RBS_util = rep(NaN, bb)
RBS_75 = rep(NaN, bb)
RBS_85 = rep(NaN, bb)
RBS_4k = rep(NaN, bb)
RBS_Sony = rep(NaN, bb)
set.seed(5)
for (ii in 1:bb) {
  RBS_y = yhat + rr[sample(nn, nn, replace = T)]
  RBS_lm = lm(RBS_y ~ x)
  RBS_beta[ii,] = RBS_lm$coefficients
  RBS_util[ii] = 500/-RBS_beta[ii,6] 
  RBS_75[ii] = RBS_beta[ii,2]*RBS_util[ii]
  RBS_85[ii] = RBS_beta[ii,3]*RBS_util[ii]
  RBS_4k[ii] = RBS_beta[ii,4]*RBS_util[ii]
  RBS_Sony[ii] = RBS_beta[ii,5]*RBS_util[ii]
}

RBS_75CI = quantile(RBS_75, probs = c(0.025, 0.5, 0.975))
RBS_85CI = quantile(RBS_85, probs = c(0.025, 0.5, 0.975))
RBS_4kCI = quantile(RBS_4k, probs = c(0.025, 0.5, 0.975))
RBS_SonyCI = quantile(RBS_Sony, probs = c(0.025, 0.5, 0.975))
RBS_CItable= rbind(RBS_75CI, RBS_85CI, RBS_4kCI, RBS_SonyCI)
RBS_CItable

DBS_beta = matrix(NaN, bb, 6)
DBS_util = rep(NaN, bb)
DBS_75 = rep(NaN, bb)
DBS_85 = rep(NaN, bb)
DBS_4k = rep(NaN, bb)
DBS_Sony = rep(NaN, bb)

for (ii in 1:bb) {
  DBS_data = data[sample(nn, nn, replace = T),]
  DBS_y = as.matrix(DBS_data[,6])
  DBS_x = as.matrix(DBS_data[,1:5])
  DBS_lm = lm(DBS_y ~ DBS_x)
  DBS_beta[ii,] = DBS_lm$coefficients
  DBS_util[ii] = 500/-DBS_beta[ii,6] 
  DBS_75[ii] = DBS_beta[ii,2]*DBS_util[ii]
  DBS_85[ii] = DBS_beta[ii,3]*DBS_util[ii]
  DBS_4k[ii] = DBS_beta[ii,4]*DBS_util[ii]
  DBS_Sony[ii] = DBS_beta[ii,5]*DBS_util[ii]
}
DBS_75CI = quantile(DBS_75, probs = c(0.025, 0.5, 0.975))
DBS_85CI = quantile(DBS_85, probs = c(0.025, 0.5, 0.975))
DBS_4kCI = quantile(DBS_4k, probs = c(0.025, 0.5, 0.975))
DBS_SonyCI = quantile(DBS_Sony, probs = c(0.025, 0.5, 0.975))
DBS_CItable = rbind(DBS_75CI, DBS_85CI, DBS_4kCI, DBS_SonyCI)


bhat = as.matrix(model$coefficients, 10, 6)
sigma = vcov(model)
MCS_bhat = mvrnorm(bb, bhat, sigma)
MCS_75 = rep(NaN, bb)
MCS_85 = rep(NaN, bb)
MCS_4k = rep(NaN, bb)
MCS_Sony = rep(NaN, bb)

for (ii in 1:bb){
  MCS_util = 500/-MCS_bhat[ii,6]
  MCS_75[ii] = MCS_bhat[ii,2] *MCS_util
  MCS_85[ii] = MCS_bhat[ii,3] *MCS_util
  MCS_4k[ii] = MCS_bhat[ii,4] *MCS_util
  MCS_Sony[ii] = MCS_bhat[ii,5] *MCS_util
}
MCS_75CI = quantile(MCS_75, probs = c(0.025, 0.5, 0.975))
MCS_85CI = quantile(MCS_85, probs = c(0.025, 0.5, 0.975))
MCS_4kCI = quantile(MCS_4k, probs = c(0.025, 0.5, 0.975))
MCS_SonyCI = quantile(MCS_Sony, probs = c(0.025, 0.5, 0.975))
MCS_CItable = rbind(MCS_75CI, MCS_85CI, MCS_4kCI, MCS_SonyCI)



RBS_CItable
DBS_CItable
MCS_CItable





