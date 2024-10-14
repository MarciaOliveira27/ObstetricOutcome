install.packages("bslib")
install.packages("cachem")
renv::snapshot()

library(rsconnect)
rsconnect::setAccountInfo(name='obstetricoutcome', 
                          token='31D13E0015A129019ADB7B38A23496F8', 
                          secret='HmAAkK9gUPJET8zQoVX0bDIatwf0FEV3Yo56+dIe')

rsconnect::deployApp('/Users/marciaoliveira/Desktop/Shiny/mo_uminho')




