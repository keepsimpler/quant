## http://blog.fosstrading.com/

# get security data from Japanese market
# YJ means Yahoo Japan
require(quantmod)
setSymbolLookup(YJ6758.T='yahooj')
getSymbols('YJ6758.T')
