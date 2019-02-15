expect_s4_class(drawdowns(t = (10^seq(2,5,length=40)), model = gfamodel("ths", aqpar = c(3E-4, 8E-4),  testdata = c(3.1E-1, 50))), "drawdowns")
data("pumptest")
mydata <- drawdowns(pumptest$agt_ds1,fn="agarwal",q=240*60,sample=TRUE, miscDdOptions=list(nval=100, df=10, defunc="spline"))
expect_s4_class(mydata, "drawdowns")
plot(mydata, derivative=TRUE)

