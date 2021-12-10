# C.p = Conversion price, specified in CoCo prospectus
# c.r = coupon rate paid on the underlying bond
# m = coupon frequency, 1 = annual, 2 = semi-annual, 4 = quarterly. # N = issue price, normally $100
# q = continuous dividend yield on the underlying stock
# r = risk free rate
# S = price of the underlying stock
# S.Trigger = implied trigger price for the CoCo to convert to equity # sigma = volatility of the underlying price
# M = maturity period, in other words, the life of the contract
CoCo.Price = function(C.p, c.r, m, N, q, r, S, S.Trigger, sigma, M){ t = seq(1/m, M, by=1/m){

  #A; Underlying Bond
  BOND = function(t, c.r, N, r, m){
    coupon = N*(c.r/m)
    c = NULL
    DF = NULL
    PV = NULL
    F = NULL
    PV.F = NULL
    for(i in 1:length(t)){
      c[i] = rep(coupon)
      DF[i] = exp(-r*t[i])
      PV[i] = DF[i]*c[i]
    }
    bond_data = cbind(t,c,DF,PV)
    for(i in length(t):length(t)){
      F[i] = N
      PV.F[i] = DF[length(t)] * F[i]
    }
    bond_data2 = data.frame(cbind(bond_data, F, PV.F))
    sum = apply(bond_data2, 2, sum)
    A = as.numeric(sum[4] + PV.F[length(t)])
  }

  #B: Long Down-and-In Forwards, DIF
  DIF = function(sigma, q, S, S.Trigger, N, r, M, C.p){ 
    C.r = N/C.p
    K = C.p

    lambda = ((r-q+(sigma^2/2))/(sigma^2))
        r1 = S/S.Trigger
        r2 = S.Trigger/S
        tv = sigma*sqrt(M)
        lv = lambda*sigma*sqrt(M)
        x1 = log(r1)/tv + lv
        y1 = log(r2)/tv + lv
        DIF = C.r*((S*exp(-q*M)*(r2^(2*lambda))*pnorm(y1))-K*exp(-r*M)*(r2^(2*lambda-2)*pnorm(y1-tv))-K*exp(-r*M)*pnorm(-x1+tv)+S*exp(-q*M)*pnorm(-x1))
  }

  #C: Short Binary Down-and-In Options, BDI
  BDI = function(sigma, c.r, r, q, S, S.Trigger, N, t){
    coupon = N*(c.r/m)
    lambda = ((r-q+(sigma^2/2))/(sigma^2))
    r1 = S/S.Trigger
    r2 = S.Trigger/S
    c = NULL
    DF = NULL
    PV = NULL
    tv.i = NULL
    lv.i = NULL
    d1.t = NULL
    d2.t = NULL
    BDI.i = NULL
    for(j in 1:length(t)){
      c[j] = rep(coupon)
      DF[j] = exp(-r*t[j])
      PV[j] = DF[j]*c[j]
      tv.i[j] = sigma*sqrt(t[j])
      lv.i[j] = lambda*sigma*sqrt(t[j])
      d1.t[j] = (log(r1)/tv.i[j]) + lv.i[j]
      d2.t[j] = (log(r2)/tv.i[j]) + lv.i[j]
      BDI.i[j] = -PV[j]*((pnorm(-d1.t[j]+tv.i[j]))
                          +((r2)^(2*lambda-2))*pnorm(d2.t[j]-tv.i[j]))
    }
    data = data.frame(cbind(t,c,DF,PV,tv.i,lv.i,d1.t,d2.t, BDI.i)) 
    BDI = sum(data$BDI.i)
  }

  # Calculate the value of the Bond
  A = BOND(t, c.r, N, r, m)
  print("The price of the bond, or A is:")
  print(A)

  # Calculate the value of the DIF
  B = DIF(sigma, q, S, S.Trigger, N, r, M, C.p)
  print("The price of the down-and-in forward, B, is:")
  print(B)

  # Calcualte the value of BDIK
  C = BDI(sigma, c.r, r, q, S, S.Trigger, N, t)
  print("The value on the short positions of the binary down-and-in options, C, is:") print(C)

  # Calculate the price of the CoCo
  print("The CoCo price is:")
  P=A+B+C
  print(P)
}

#Test
S = 5 
S.Trigger = 3 
c.r = 0.05 
C.p = 6
M = 5
q = 0.02
r = 0.02
N = 100
sigma = 0.3
TEST = CoCo.Price(C.p, c.r, m, N, q, r, S, S.Trigger, sigma, M)
par(mfrow=(c(1,4)))

#This section maps the function to changes in the underyling stock price to check the delta and gamma.
Si = seq(0.5, 15, 0.01)
DeltaGamma = NULL
for(i in 1:length(Si)){
  DeltaGamma[i] = CoCo.Price(C.p, c.r, m, N, q, r, Si[i], S.Trigger, sigma, M)
}
plot(Si, DeltaGamma, cex.main=2, main = "Delta + Gamma", type = "l", xlab = "Underlying price", ylab = "CoCo Price", col = "blue", lwd = 2)

#This section maps the function to changes in volatility of the underlying to check the vega and volga
sigmai = seq(0.05, 1.5, 0.001)

length(Si)
VegaVolga = NULL
for(i in 1:length(Si)){
  VegaVolga[i] = CoCo.Price(C.p, c.r, m, N, q, r, S, S.Trigger, sigmai[i], M) 
}
plot(sigmai, cex.main=2, VegaVolga, main = "Vega + Volga", type ="l", xlab="Volatility", ylab = "CoCo Price", col = "red", lwd = 2)

#This section maps the function to changes in conversion prices to evaluate the impact upon stakeholders
CPi = seq(5,100,0.1)
CPSens = NULL
for(i in 1:length(CPi)){
  CPSens[i] = CoCo.Price(CPi[i], c.r, m, N, q, r, S, S.Trigger, sigma, M)
}
plot(CPi, CPSens, cex.main=2, main = "CP Sensitivity", type ="l", xlab ="Conversion Price", ylab = "CoCo Price", col = "lightblue", lwd = 2)

##This section maps the function to changes in time to maturity
Mi = seq(0.5, 15, 0.01)
MSens = NULL
for(i in 1:length(Mi)){
  MSens[i] = CoCo.Price(C.p, c.r, m, N, q, r, S, S.Trigger, sigma, Mi[i]) 
}
plot(Mi, MSens, cex.main=2, main = "Maturity Sensitivity", type ="l", xlab ="Time to Maturity", ylab = "CoCo Price", col = "green", lwd = 2)

#This section maps the function to changes in coupon rates.
CRi = seq(0.01, 0.1, 0.0001)
CRSens = NULL
for(i in 1:length(CRi)){
  CRSens[i] = CoCo.Price(C.p, CRi[i], m, N, q, r, S, S.Trigger, sigma, M) 
}
plot(CRi, CRSens, main = "CR Sensitivity", type ="l", xlab ="Coupon rate", ylab = "CoCo Price", col = "orange", lwd = 2)
