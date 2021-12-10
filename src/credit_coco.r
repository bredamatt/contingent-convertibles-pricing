#CoCo Spread under the credit derivatives approach
CoCo_spread = function(FaceValue,St,Trigger,rf,DivYield,Volatility,Maturity,ConvPrice){ 
  N = FaceValue
  x = Trigger
  y = St
  sig = Volatility
  M = Maturity
  CP = ConvPrice
  mao = rf-DivYield-(sig^2) /2
  d1 = ((log(x/y))-mao*M)/(sig*sqrt(M))
  d2 = ((log(x/y))+mao*M)/(sig*sqrt(M))
  pstar = pnorm(d1) + ((x/y)^((2*mao)/sig^2))*pnorm(d2)
  print("pstar")
  print(pstar)
  lambda = -(log(1-pstar))/M
  print("lambda")
  print(lambda)
  RR = x/CP
  print(RR)
  spread = (1-RR)*lambda
  print("spread")
  print(spread)
  return(spread)
}

#Initial parameters:
TTM = 10
St = 100 sigma = 0.3 q=0
rf = 0.04 Trigger = 50 CP = St
N = 100
par(mfrow=c(1,4))

#Plots for Figure 6
#Time to maturity
#Underlying price
#Volatility
#Dividend yield
#Continuous risk free rate
#Trigger price
#Conversion price
#Face value

Voli = seq(0, 1, 0.01)
S1 = NULL
for(i in 1:length(Voli)){
  S1[i] = CoCo_spread(FaceValue=N, St=St,Trigger=Trigger, rf=rf, DivYield=q, Voli[i], Maturity = TTM, ConvPrice = St)
}
plot(Voli, S1, type="l", lwd=2, cex.main=2, col ="red", xlab ="Volatility", ylab ="CoCo Spread", main = "Vega + Volga")

S = seq(45, 150, 0.01)
S2 = NULL
for(i in 1:length(S)){
  S2[i] = CoCo_spread(FaceValue=N, St=S[i],Trigger=Trigger, rf=rf, DivYield=q, sigma, Maturity = TTM, ConvPrice = St)
}
plot(S,S2, type ="l", lwd ="2", xlim = c(50,150), cex.main=2, col ="blue", xlab = "Underlying Price", ylab ="CoCo Spread", main = "Delta + Gamma")

Trig = seq(10, 110, .25)
S3 = NULL
for(i in 1:length(Trig)){
  S3[i] = CoCo_spread(FaceValue=N, St=St,Trigger=Trig[i], rf=rf, DivYield=q, sigma, Maturity = TTM, ConvPrice = St)
}
plot(Trig,S3, type ="l", lwd="2", cex.main=2, col = "black", xlab = "Trigger Price", ylab = "CoCo Spread", main = " S* Sensitivity")

#Maturity
TTMi = seq(0,7,0.5)
S4 = NULL
for(i in 1:length(TTMi)){
  S4[i] = CoCo_spread(FaceValue=N, St=St,Trigger=Trigger, rf=rf, DivYield=q, sigma, Maturity = TTMi[i], ConvPrice = CP)
}
plot(TTMi, S4, type = "l", lwd="2", cex.main=2, col = "green", xlab = "Time to Maturity", ylab = "CoCo Spread", main = "Maturity Sensitivity")
