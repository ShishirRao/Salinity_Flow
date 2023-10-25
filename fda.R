library(fda)

# Expression or formula
f = expression(4+ 6*sin(pi*t/6)+ 7*cos(pi*t/6))

cat("\nUsing D() function:\n")
print(D(f, 't'))
f1 = (D(f, 't'))


f2 = D(f1, 't')
print(f2)

f3 = D(f2, 't')
print(f3)

Lf = expression((pi/6)^2 * f1 + f3)


annualprec = log10(apply(daily$precav,2,sum))
?apply

tempbasis =create.fourier.basis(c(0,365),65)
tempSmooth=smooth.basis(day.5,daily$tempav,tempbasis)
tempfd =tempSmooth$fd
