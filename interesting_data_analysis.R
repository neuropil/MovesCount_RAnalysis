require(ggplot2)
require(RCurl)
require(mapproj)

#gsqAPI is a helper function that loads data in from a shared as public Google Spreadsheet.
gsqAPI = function(key,query,gid=0){ 
  return( read.csv( paste( sep="",'http://spreadsheets.google.com/tq?', 
                           'tqx=out:csv','&tq=', curlEscape(query), '&key=', key, '&gid=', gid) ) ) }



#Provide the spreadsheet key
#Data was originally grabbed from the McLaren F1 Live Dashboard during the race and is 
#Copyright McLaren Marketing Ltd 2010 (I think? Or possibly Vodafone McLaren Mercedes F1 2010. 
#I believe that speed, throttle and brake data were sponsored by Vodafone.
key='0AmbQbL4Lrd61dER5Qnl3bHo4MkVNRlZ1OVdicnZnTHc'
#We can write SQL like queries over the spreadsheet,as described in 
#http://blog.ouseful.info/2009/05/18/using-google-spreadsheets-as-a-databace-with-the-google-visualisation-api-query-language/
q='select *'

#Run the query on the database
df=gsqAPI(key,q)

#Sanity check - preview the imported data
head(df)

#Example circuit map - sort of - showing the gLat (latitudinal 'g-force') 
#values around the circuit (point size is absolute value of gLat, colour 
#has two values, one for + and one for - values (swing to left and swing to right)).
g = ggplot(df) + geom_point(aes(x=NGPSLongitude,y=NGPSLatitude,col=sign(gLat),size=abs(gLat)))
print(g)

ggplot(df) + geom_point(aes(x=NGPSLongitude,y=NGPSLatitude,col=sign(gLat),size=abs(gLat))) + 
  coord_map(project="mercator")


#Example "driver DNA" trace, showing low gear  throttle usage (distance round track on x-axis, lap number on y axis, node size is inversely proportional to gear number (low gear, large point size), colour relativ to throttlepedal depression
g=ggplot(df) + geom_point(aes(x=sLap,y=Lap,col=rThrottlePedal,size=-NGear)) + scale_colour_gradient(low='red',high='green')
print(g)

#Example of gear value around the track
g=ggplot(df) + geom_line(aes(x=sLap,y=NGear))
print(g)



#We can also show a trace for a single lap, such as speed coloured by gear
g=ggplot(subset(df,Lap==22)) + geom_line(aes(x=sLap,y=vCar,colour=NGear))
print(g)


#We can also do statistical graphics - like a boxplot showing the distribution of speed values by gear
g = ggplot(df) + geom_boxplot(aes(factor(NGear),vCar))
print(g)


#Footwork - brake and throttle pedal depression based on gear
g = ggplot(df) + geom_jitter(aes(factor(NGear),rThrottlePedal),colour='darkgreen') + geom_jitter(aes(factor(NGear),pBrakeF),colour='darkred')
print(g)


#Forces on the driver
#gLong by brake and gear
g = ggplot(df) + geom_jitter(aes(factor(NGear),gLong,col=pBrakeF)) + scale_colour_gradient(low='red',high='green')
print(g)

#gLong by throttle and gear
g = ggplot(df) + geom_jitter(aes(factor(NGear),gLong,col=rThrottlePedal)) + scale_colour_gradient(low='red',high='green')
print(g)

#gLong boxplot
ggplot(df) + geom_boxplot(aes(factor(NGear),gLong))+ geom_jitter(aes(factor(NGear),gLong),size=1)

#How do engine revs and speed relate to gear selction?
ggplot(df)+geom_point(aes(x=nEngine,y=vCar,col=factor(NGear)))
