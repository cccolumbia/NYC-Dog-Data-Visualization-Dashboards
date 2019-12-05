#read data and pre-processing data
dogbites=read.csv("dogbites.csv")
doglicences=read.csv("doglicences.csv")
dogruns=read.csv("dogruns.csv")
parkdir=read.csv("parkdirectory.csv")
parkdir$Zip=as.character(parkdir$Zip)
parkdir$Zip=sub(", .*","",parkdir$Zip)
parkdir$Zip=as.numeric(parkdir$Zip)
str(dogbites)
str(doglicences)
str(dogruns)



##############A2 & A3##################
dogruns["plot_name"]=dogruns$Name
dogruns$plot_name=sub(" Dog Run","",dogruns$plot_name)
dogruns$plot_name=sub(" Off-Leash Area","",dogruns$plot_name)
dogruns$plot_name=paste(dogruns$plot_name,", New York",seq="")

#libraries
library(ggplot2)
library(ggmap)
library(tidyverse)
library(maps)

#geocoding address
  # dogruns["lon"]=NULL
  # dogruns["lat"]=NULL
  # for(i in 1:nrow(dogruns))
  # {
  #   # Print("Working...")
  #   result <- geocode(dogruns$plot_name[i], output = "latlon",
  #                     source = "google")
  #   dogruns$lon[i] <- as.numeric(result[1])
  #   dogruns$lat[i] <- as.numeric(result[2])
  # }
  # write.csv(dogruns,"dogruns2.csv")


# for this plot, I upload a picture. scatter plot of run areas on map
register_google(key="AIzaSyBbJbhg97t5oEGf9m39krg9Dz-8LkB9bkI")
nyc_base <- ggmap::get_map("New York City", zoom = 10)
ggmap(nyc_base) + geom_point(data=dogruns, aes(x=lon, y=lat),
                             color="purple", size=2, alpha=0.5,stroke=0)

#create Borough coloumn
  # dogruns["Borough"]=NULL
  # dogruns['Zipcode']=NULL
  # for(i in 1:nrow(dogruns))
  # {
  #   res <- geocode(as.character(dogruns[i,"plot_name"]), output = 'all', source = 'google')
  #   code=as.character(res$results[[1]]
  #                     $address_components[[length(res$results[[1]]$address_components)]]
  #                     $long_name)
  #   borough="None"
  #   dogruns[i,"Zipcode"]=code
  #   if(code>10000 & code<10283) borough="Manhatten"
  #   if(code>10450 & code<10476) borough="Bornx"
  #   if(code>11003 & code<11110) borough="Queens"
  #   if(code>11350 & code<11698) borough="Queens"
  #   if(code>11200 & code<11257) borough="Brooklyn"
  #   if(code>11200 & code<11257) borough="Brooklyn"
  #   if(code>10300 & code<10315) borough="Staten"
  #   dogruns[i,"Borough"]=borough
  # }
  # write.csv(dogruns,"dogruns2.csv")

dogruns=read.csv("dogruns2.csv")
dogruns=na.omit(dogruns)

ggplot(dogruns, aes(fct_infreq(Borough)))+
  geom_bar(fill = "lightblue",color="lightblue")+
  xlab("Borough")

################ A16 ####################

ggplot(dogruns[dogruns["Zipcode"]!="United States",], aes(Zipcode))+
  geom_bar(fill = "lightblue",color="lightblue")+
  xlab("Zipcode")+
  theme(axis.text.x = element_text(size = 10,hjust=1,
                                   vjust = 1, angle = 45))


################# B8 ##################
library(googleway)
  # dogbites["lan"]=NULL
  # dogbites["lon"]=NULL
  # for(i in 1:nrow(dogbites))
  #   {
  #     res <- geocode(as.character(dogbites[i,"ZipCode"]),
  #                    output = 'latlon', source = 'google')
  #     dogbites[i,"lon"]=res[1]
  #     dogbites[i,"lat"]=res[2]
  #     }
  # write.csv(dogbites,"dogbites2.csv")

dogbites=read.csv("dogbites2.csv")
dogbites=na.omit(dogbites)
dogbites=dogbites[dogbites$lon<(-70)&dogbites$lon>(-75)&dogbites$lat>39&dogbites$lat<42,]

#need to replace value of key
key <- "AIzaSyBbJbhg97t5oEGf9m39krg9Dz-8LkB9bkI"

google_map(data = dogruns,key=key) %>%
  add_heatmap(lat="lat", lon="lon", option_radius = 0.01)

################# E1 #################
nyc_base <- ggmap::get_map("New York City", zoom = 10)
ggmap(nyc_base) + 
  geom_point(data=dogbites, aes(x=lon, y=lat),
                             color="red", size=1, alpha=0.5,stroke=0)+
  geom_point(data=dogruns, aes(x=lon, y=lat),
               color="purple", size=3, alpha=0.5,stroke=0)
