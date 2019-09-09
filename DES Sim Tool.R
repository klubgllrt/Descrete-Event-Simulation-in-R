#http://www.bupar.net/index.html
#bupaR link provided above
#Contact Caleb gellert about set up or program issues
#csgeller@ncsu.edu
#(910) 603-0109

######Installation######

#  install.packages("bupaR")
# install.packages("edeaR")
# install.packages("eventdataR")
# install.packages("processmapR")
# install.packages("processmonitR")
# install.packages("xesreadR")
# install.packages("petrinetR")
# install.packages("simmer", dependencies = T)

#clears list logs
rm(list=ls(all=TRUE))


library(simmer)


# instantiate the environment
IMIM <- simmer()

#Set Backlog Volume
Vol = 10
SIMtime = 400
SIMstartDATE = "2018-08-15"

    ##
  ####
 #####  -> Ideally I would like to pull the distributions from real data 
  ####  -> and offer the oppurtunity to maunally input the data
  ####  -> Currently, I have everything hard coded
  ####  
  ####
#######



Val0 = 1/.001 #Enter_RFP
Val1 = 1/.01   #Verify Part
Val2 = 1/.01 #Commodity Code Review
Val3 = 1/3 #handle Problem Part
Val4 = 1/10 #Manufacturing Plan
Val5 = 1/4 #initial Lead time
Val6 = 1/2 #No Bid Review
Val7 = 1/15 #Develop BOE 
Val8 = 1/5 #Perform Lead time analysis
Val9 = 1/20 #Process IWA
Val10 = 1/25 #Assign Buyer
Val11 = 1/30 #Get New Supplier Quote
Val12 = 1/15 #Perform BOE Review
Val13 = 1/10 #transfer to SPS Pricing
Val14 = 1/11 #wait for sps pricing
Val15 = 1/5 #prepare for review
Val16 = 1/5 #review proposal 
Val17 = 1/2 #approved proposal 
Val18 = 1/2 #rejected proposal 
Val19 = 1/2 #review bid gate request
Val20 = 1/10 #no bid approved
Val21 = 1/10 #CCS final push attach header


#headcount assumptions
HC_DLA= 25 
HC_SOS= 5
HC_CnP= 30
HC_ME= 50
HC_IWA= 200
HC_OM= 25
HC_PFA= 25
HC_SM= 25

   ####
 ### ####
##     ###  -> Ideally I would like to pull the classifications and breakdowns from real data 
      ###  -> I would also like to add in some kind of variance in the percentages
     ###  -> Currently, I have everything hard coded
   ####  
  ####    #
###########

#Current breakdown from Jay & Sarena- From Digital Ascent file. This can be found in the resources Folder

NoPropVol= Vol*.15
SOFSVol= Vol*.14
IWAVol = Vol*.12
HISTVol = Vol*.47
QUOTEVol = Vol*.11



    ####
  ### ####
  #    ###  -> I would like to run the scenario through a monte carlo & produce multiple runs to compare against
     ###  
      ###  -> Currently, I have just one seed hard coded
      ####  
 ##  ####    
  ##### 

set.seed(101) #Random Draw



NoProposalOrders <- trajectory() %>%
  
  log_(function() {paste(",start,Enter_RFP,", now(IMIM), ",SOS,")})  %>%
  seize("SOS",1) %>%
 # #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val0)}) %>%
  release("SOS",1) %>%
  log_(function() {paste(",complete,Enter_RFP,", now(IMIM),  ",SOS,")}) %>%
  
  log_(function() {paste(",start,Verify Part,", now(IMIM), ",SOS,")})  %>%
  seize("SOS",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val1)}) %>%
  release("SOS",1) %>%
  log_(function() {paste(",complete,Verify Part, ", now(IMIM), ",SOS,")}) %>%

  
  log_(function() {paste(",start,No-Bid Review,", now(IMIM), ",C&P,")}) %>%
  seize("C&P",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val6)}) %>%
  release("C&P",1) %>%
  log_(function() {paste(",complete,No-Bid Review, ", now(IMIM),  ",C&P,")}) %>%
  
  
  log_(function() {paste(",start,No-Bid Approved,", now(IMIM), ",SOS,")}) %>%
  seize("SOS",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val20)}) %>%
  release("SOS",1) %>%
  log_(function() {paste(",complete,No-Bid Approved, ", now(IMIM), ",SOS,")}) 

SOFSBOE <- trajectory() %>%

  
    

      #  
     ##
    ###
   ## #    -> I would like to create a priority scheme similar to the one created for the RFPs
  ##  #
########   -> Currently, every proposal is wieghted by the same priority
    ###    -> In the two blocks below I have priority code writted but it is commented and I have not messed with implementing it
    ###     
   #####

#Priority
# set_attribute("priority", 3) %>%
# # static values
# set_prioritization(values = c(3, 7, TRUE)) %>%
# # dynamically with a function
# set_prioritization(values = function() {
#   prio <- get_prioritization(env)
#   attr <- get_attribute(env, "priority")
#   c(attr, prio[[2]]+1, FALSE)
# })

  log_(function() {paste(",start,Enter_RFP,", now(IMIM), ",SOS,")})  %>%
  set_attribute("Enter_RFP", function() {now(IMIM)}) %>%
  #select(c("counter1", "counter2"), policy = "shortest-queue") %>%
  seize("SOS",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val0)}) %>%
  release("SOS",1) %>%
  log_(function() {paste(",complete,Enter_RFP,", now(IMIM),  ",SOS,")}) %>%
  
  
  log_(function() {paste(",start,Prepare For Review,", now(IMIM), ",C&P,")}) %>%
  seize("C&P",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val15)}) %>%
  release("C&P",1) %>%
  log_(function() {paste(",complete,Prepare For Review,", now(IMIM),  ",C&P,")}) %>%
  
  
  log_(function() {paste(",start,Review Proposal,", now(IMIM), ",C&P,")}) %>%
  seize("C&P",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val16)}) %>%
  release("C&P",1) %>%
  log_(function() {paste(",complete,Review Proposal,", now(IMIM), ",C&P,")}) %>%
  
  
  log_(function() {paste(",start,Approved Proposal,", now(IMIM), ",C&P,")})%>%
  seize("C&P",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val17)}) %>%
  release("C&P",1) %>%
  log_(function() {paste(",complete,Approved Proposal,", now(IMIM),  ",C&P,")})

HistoryBOE <- trajectory() %>%


  log_(function() {paste(",start,Enter_RFP,", now(IMIM), ",SOS,")})  %>%
  set_attribute("Enter_RFP", function() {now(IMIM)}) %>%
  seize("SOS",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val0)}) %>%
  release("SOS",1) %>%
  log_(function() {paste(",complete,Enter_RFP,", now(IMIM),  ",SOS,")}) %>%
  
  log_(function() {paste(",start,Verify Part,", now(IMIM), ",SOS,")}) %>%
  seize("SOS",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val1)}) %>%
  release("SOS",1) %>%
  log_(function() {paste(",complete,Verify Part,", now(IMIM),  ",SOS,")}) %>%  
  
  log_(function() {paste(",start,Manufacturing Plan,", now(IMIM), ",ME,")}) %>%
  seize("ME",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val4)}) %>%
  release("ME",1) %>%
  log_(function() {paste(",complete,Manufacturing Plan,", now(IMIM),  ",ME,")}) %>%
  
  log_(function() {paste(",start,Initial Lead Time,", now(IMIM), ",OM,")}) %>%
  seize("OM",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, ",start_time"))}) %>%
  timeout(function() {rexp(1, Val5)}) %>%
  release("OM",1) %>%
  log_(function() {paste(",complete,Initial Lead Time,", now(IMIM),  ",OM,")}) %>%
  
  log_(function() {paste(",start,Lead Time,", now(IMIM), ",OM,")}) %>%
  seize("OM",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val8)}) %>%
  release("OM",1) %>%
  log_(function() {paste(",complete,Lead Time,", now(IMIM),  ",OM,")})%>%
  
  log_(function() {paste(",start,Devlop BOE,", now(IMIM), ",PFA,")}) %>%
  seize("PFA",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val7)}) %>%
  release("PFA",1) %>%
  log_(function() {paste(",complete,Devlop BOE,", now(IMIM),  ",PFA,")}) %>%
  
  log_(function() {paste(",start,Transfer to SPS,", now(IMIM), ",C&P,")}) %>%
  seize("C&P",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val13)}) %>%
  release("C&P",1) %>%
  log_(function() {paste(",complete,Transfer to SPS,", now(IMIM),  ",C&P,")}) %>%
  
  log_(function() {paste(",start,Prepare For Review,", now(IMIM), ",C&P,")}) %>%
  seize("C&P",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val15)}) %>%
  release("C&P",1) %>%
  log_(function() {paste(",complete,Prepare For Review,", now(IMIM),  ",C&P,")}) %>%
  
  log_(function() {paste(",start,Review Proposal,", now(IMIM), ",C&P,")}) %>%
  seize("C&P",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val16)}) %>%
  release("C&P",1) %>%
  log_(function() {paste(",complete,Review Proposal,", now(IMIM),  ",C&P,")}) %>%
  
  log_(function() {paste(",start,Approved Proposal,", now(IMIM), ",C&P,")})%>%
  seize("C&P",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val17)}) %>%
  release("C&P",1) %>%
  log_(function() {paste(",complete,Approved Proposal,", now(IMIM),  ",C&P,")})

NewBOERequiredIWA <- trajectory() %>%
  

  log_(function() {paste(",start,Enter_RFP,", now(IMIM), ",SOS,")})  %>%
  set_attribute("Enter_RFP", function() {now(IMIM)}) %>%
  seize("SOS",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val0)}) %>%
  release("SOS",1) %>%
  log_(function() {paste(",complete,Enter_RFP, ", now(IMIM),  ",SOS,")}) %>%
  
  log_(function() {paste(",start,Verify Part,", now(IMIM), ",SOS,")}) %>%
  seize("SOS",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val1)}) %>%
  release("SOS",1) %>%
  log_(function() {paste(",complete,Verify Part, ", now(IMIM),  ",SOS,")}) %>%  
  
  
  log_(function() {paste(",start,Manufacturing Plan,", now(IMIM), ",ME,")}) %>%
  seize("ME",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val4)}) %>%
  release("ME",1) %>%
  log_(function() {paste(",complete,Manufacturing Plan,", now(IMIM),  ",ME,")}) %>%
  
  log_(function() {paste(",start,Initial Lead Time,", now(IMIM), ",OM,")}) %>%
  seize("OM",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, ",start_time"))}) %>%
  timeout(function() {rexp(1, Val5)}) %>%
  release("OM",1) %>%
  log_(function() {paste(",complete,Initial Lead Time,", now(IMIM),  ",OM,")}) %>%
  
  log_(function() {paste(",start,Process IWA,", now(IMIM), ",IWA,")}) %>%
  seize("IWA",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val9)}) %>%
  release("IWA",1) %>%
  log_(function() {paste(",complete,Process IWA,", now(IMIM),  ",IWA,")})%>%
  
  log_(function() {paste(",start,Lead Time,", now(IMIM), ",OM,")}) %>%
  seize("OM",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val8)}) %>%
  release("OM",1) %>%
  log_(function() {paste(",complete,Lead Time,", now(IMIM),  ",OM,")})%>%
  
  
  log_(function() {paste(",start,Devlop BOE,", now(IMIM), ",PFA,")}) %>%
  seize("PFA",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val7)}) %>%
  release("PFA",1) %>%
  log_(function() {paste(",complete,Devlop BOE,", now(IMIM),  ",PFA,")}) %>%
  
  log_(function() {paste(",start,Transfer to SPS,", now(IMIM), ",C&P,")}) %>%
  seize("C&P",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val13)}) %>%
  release("C&P",1) %>%
  log_(function() {paste(",complete,Transfer to SPS,", now(IMIM),  ",C&P,")}) %>%
  
  log_(function() {paste(",start,Prepare For Review,", now(IMIM), ",C&P,")}) %>%
  seize("C&P",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val15)}) %>%
  release("C&P",1) %>%
  log_(function() {paste(",complete,Prepare For Review,", now(IMIM),  ",C&P,")}) %>%
  
  log_(function() {paste(",start,Review Proposal,", now(IMIM), ",C&P,")}) %>%
  seize("C&P",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val16)}) %>%
  release("C&P",1) %>%
  log_(function() {paste(",complete,Review Proposal,", now(IMIM),  ",C&P,")}) %>%
  
  log_(function() {paste(",start,Approved Proposal,", now(IMIM), ",C&P,")})%>%
  seize("C&P",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val17)}) %>%
  release("C&P",1) %>%
  log_(function() {paste(",complete,Approved Proposal,", now(IMIM),  ",C&P,")})

NewBOERequiredQuote <- trajectory() %>%

  log_(function() {paste(",start,Enter_RFP,", now(IMIM), ",SOS,")})  %>%
  set_attribute("Enter_RFP", function() {now(IMIM)}) %>%
  seize("SOS",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val0)}) %>%
  release("SOS",1) %>%
  log_(function() {paste(",complete,Enter_RFP,", now(IMIM),  ",SOS,")}) %>%
  
  log_(function() {paste(",start,Verify Part,", now(IMIM), ",SOS,")}) %>%
  seize("SOS",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val1)}) %>%
  release("SOS",1) %>%
  log_(function() {paste(",complete,Verify Part, ", now(IMIM),  ",SOS,")}) %>%  
  
  
  log_(function() {paste(",start,Manufacturing Plan,", now(IMIM), ",ME,")}) %>%
  seize("ME",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val4)}) %>%
  release("ME",1) %>%
  log_(function() {paste(",complete,Manufacturing Plan,", now(IMIM),  ",ME,")}) %>%
  
  log_(function() {paste(",start,Initial Lead Time,", now(IMIM), ",OM,")}) %>%
  seize("OM",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, ",start_time"))}) %>%
  timeout(function() {rexp(1, Val5)}) %>%
  release("OM",1) %>%
  log_(function() {paste(",complete,Initial Lead Time,", now(IMIM),  ",OM,")}) %>%
  
  log_(function() {paste(",start,Get Supplier Quote,", now(IMIM), ",OM,")}) %>%
  seize("SM",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val11)}) %>%
  release("SM",1) %>%
  log_(function() {paste(",complete,Get Supplier Quote,", now(IMIM),  ",OM,")}) %>%
  
  log_(function() {paste(",start,Lead Time,", now(IMIM), ",OM,")}) %>%
  seize("OM",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val8)}) %>%
  release("OM",1) %>%
  log_(function() {paste(",complete,Lead Time,", now(IMIM),  ",OM,")})%>%
  
  
  log_(function() {paste(",start,Devlop BOE,", now(IMIM), ",PFA,")}) %>%
  seize("PFA",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val7)}) %>%
  release("PFA",1) %>%
  log_(function() {paste(",complete,Devlop BOE,", now(IMIM),  ",PFA,")}) %>%
  
  log_(function() {paste(",start,Transfer to SPS,", now(IMIM), ",C&P,")}) %>%
  seize("C&P",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val13)}) %>%
  release("C&P",1) %>%
  log_(function() {paste(",complete,Transfer to SPS,", now(IMIM),  ",C&P,")}) %>%
  
  log_(function() {paste(",start,Prepare For Review,", now(IMIM), ",C&P,")}) %>%
  seize("C&P",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val15)}) %>%
  release("C&P",1) %>%
  log_(function() {paste(",complete,Prepare For Review,", now(IMIM),  ",C&P,")}) %>%
  
  log_(function() {paste(",start,Review Proposal,", now(IMIM), ",C&P,")}) %>%
  seize("C&P",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val16)}) %>%
  release("C&P",1) %>%
  log_(function() {paste(",complete,Review Proposal,", now(IMIM),  ",C&P,")}) %>%
  
  log_(function() {paste(",start,Approved Proposal,", now(IMIM), ",C&P,")})%>%
  seize("C&P",1) %>%
  #log_(function() {paste("Waited: ", now(IMIM) - get_attribute(IMIM, "start_time"))}) %>%
  timeout(function() {rexp(1, Val17)}) %>%
  release("C&P",1) %>%
  log_(function() {paste(",complete,Approved Proposal,", now(IMIM),  ",C&P,")})

IMIM <-
  simmer("IMIM") %>%

#adds employees  
  add_resource("DLA", HC_DLA) %>%
  add_resource("SOS", HC_SOS) %>%
  add_resource("C&P", HC_CnP) %>%
  add_resource("ME", HC_ME) %>%
  add_resource("IWA", HC_IWA) %>%
  add_resource("OM", HC_OM) %>%
  add_resource("PFA", HC_PFA) %>%
  add_resource("SM", HC_SM) %>%
  
  
#adds in RFP trajectories  [Processes]
  add_generator("NoProposalOrders", NoProposalOrders, function() {c(0, rexp(NoPropVol, 1/10), -1)}) %>% #first number after c is the volume of RFPs
  add_generator("SOFSBOE", SOFSBOE, function() {c(Vol, rexp(SOFSVol, 1/10), -1)}) %>% #first number after c is the volume of RFPs
  add_generator("NewBOERequiredIWA", NewBOERequiredIWA, function() {c(0, rexp(IWAVol, 1/10), -1)}) %>% #first number after c is the volume of RFPs
  add_generator("NewBOERequiredQuote", NewBOERequiredQuote, function() {c(0, rexp(QUOTEVol, 1/10), -1)}) %>% #first number after c is the volume of RFPs
  add_generator("HistoryBOE", HistoryBOE, function() {c(0, rexp(HISTVol, 1/10), -1)}) #first number after c is the volume of RFPs


#Adding title to log file
sink("SIM.txt", append = FALSE, "output", split=TRUE)

#Sim Time Limit
IMIM %>% run(until = SIMtime)

sink()

# IMIM %>%
#   get_mon_arrivals %>%
#   dplyr::mutate(service_start_time = end_time - activity_time) %>%
#   dplyr::arrange(start_time)
# 
# IMIM %>%
#   get_mon_resources %>%
#   dplyr::arrange(time)

#Total Thruput & Idle Time 
result <-
  IMIM %>%
  get_mon_arrivals %>%
  dplyr::mutate(Idle_time = end_time - start_time - activity_time)

#out <- monitor_csv("C:\Users\fb350e\Documents")

# library(simmer.plot)
# 
# get_palette <- scales::brewer_pal(type = "qual", palette = 1)
# plot(SOFSBOE, fill = get_palette)




#############################Activity Info Log File

#read log
(txt <- readLines("SIM.txt"))

# detect lines starting with an arrow over
I <- grepl(">", txt)

# and throw them out
(dat <- txt[!I])

#parse away colon
(standardfields <- strsplit(dat, split = ": "))
#delimit on Comma
(standardfields1 <- strsplit(as.character(standardfields), split = ","))

#identify irrelevant data i.e. R output messages
i <- length(standardfields1)
J<-c()
x= 0
for(x in 1:i){
  #print(x)
  J[x] <- grepl("8", length(standardfields1[[x]]))
  #print(J)
  x=x+1
}

#throw out irrelevant data
(standardfields2 <- standardfields1[J])

#convert list to dataframe
df<-data.frame(Reduce(rbind, standardfields2))
#name columns
colnames(df) <-c("ERRtimestamp","activity_id","blank","status","activity","timestamp","resource","activity_instance") 
#convert time delta to Date
df$timestamp <- as.Date(SIMstartDATE)  + as.numeric(as.character(df$timestamp))



require(tcltk)
df$activity_instance <- ""
i=nrow(df)
j=nrow(df)
k=0
x=0
y=0
z=1

for(y in 1:j)
{#1
     if (df$status[y]=="start")
       {#2
        k=k+1
        df$activity_instance[y] = k
     }#2
    else
       {#3
         # msgBox <- tkmessageBox(title = "1",
         #                        message = as.character(df$activity_id[y]), icon = "info", type = "ok") 
         for(x in 1:i)
         {#4
           if(df$activity_id[y] == df$activity_id[x] && df$activity_instance[x] == "")
           {#5

             for(z in 1:x)
              {#6
               if(df$activity_id[x]==df$activity_id[z] && df$activity[x]==df$activity[z])
                  {#7
                 
                  df$activity_instance[x]<- df$activity_instance[z]
                  #z=x
                 } #7
              }#6 
           }#5
         }#4
       }#3

}#1








#keep only the relevant columns
keeps<- c("activity_id","activity","status","timestamp","resource", "activity_instance")
#store relevant columns in a new dataframe
DF<-df[keeps]



######Load BuPartool######
library(bupaR)


# ######Convert timestamp to date######
DF$timestamp = as.POSIXct(DF$timestamp) # convert imported columns to time



ev = DF %>% #a data.frame with the information
  # mutate(#status = "complete",
  #   activity_instance = 1:nrow(.)) %>%
  eventlog(
    case_id = "activity_id",
    activity_id = "activity",
    activity_instance_id = "activity_instance",
    lifecycle_id = "status",
    timestamp = "timestamp",
    resource_id = "resource",
    order= 'auto'
  )

# ev %>% summary
# ev %>% n_activities
# ev %>% n_activity_instances
# ev %>% n_events
# ev %>% n_traces
# ev %>% n_resources
# ev %>% n_cases


# CSVout = ev %>% durations
# write.csv(CSVout,'durations_Info_SIM_test.csv')

##sampling

# ev %>%
#   sample_n(size = 10)

#######Time Perspective#######

# Idle Time
# ev %>%
#   idle_time("log", units = "days") %>%
#   plot


# ev %>%
#   redo_repetitions_referral_matrix(type = "absolute") %>%
#   plot

#Processing Time
# ev %>%
#   processing_time("activity", units = "days") %>%
#   plot

# CSVout = ev %>% processing_time("activity", units = "days")
# write.csv(CSVout,'Activity_Info_SIM_test.csv')

# ev %>%
#   processing_time("activity", units = "days") 
#   plot
#Throughput TIme
ev %>%
  throughput_time("log", units= "days") %>%
  plot()

######Organizational Perspective#######

#Resource Frequency
ev %>%
  resource_frequency("resource") %>%
  plot()

#Resource involvement
# ev %>%
#   resource_involvement("resource") %>% plot

#Resource Specialization
# ev %>%
#   resource_involvement("resource") %>% plot


######Structuredness######

#Activity Presece
# ev %>% activity_presence() %>%
#   plot

# ev %>%
#   activity_frequency("activity")

# start Events
# ev %>%
#   start_activities("resource-activity")

# End Events
# ev %>%
#   end_activities("resource-activity")

## Trace Coverage
# ev %>%
#   trace_coverage("trace") %>%
#   plot()

## Trace Length
# ev %>%
#   trace_length("log") %>%
#   plot

##Rework
#Documentation coming soon

######Subsetting event data######

##Filter activities##
# ev %>%
# filter_activity(c("1.Verify Part", "21.CCS Final Push Attach Header")) %>%
# summary

##Filter on activity frequency##
# ev %>%
# filter_activity_frequency(percentile_cut_off = 0.5, reverse = T) %>%
# activity_frequency("activity")

##Filter on attributes##
# ev %>%
# filter_attributes(employee == "r1" | handling == "X-Ray") 

##Filter on resource##
# ev %>%
# filter_resource(c("r1","r4")) %>%
# resource_frequency("resource")

##TRim Cases##
# ev %>%
#   filter_trim(start_activities = "Registration", end_activities =  c("MRI SCAN","X-Ray")) %>%
#   process_map(type = performance())

##Case Filters##
# Coming Soon

######Process map######

##Frequency##
# ev %>%
#  process_map()

##Relative Frequency##
# ev %>%
#  process_map(type = frequency("relative"))

##Performance profile##
ev %>%
 process_map(performance(median, "days"))

##Customizing Process Maps##
#ev %>%
#   process_map(render = F)

# #####Resouce Map######
# ev %>%
#   resource_map(performance(median, "days"))

######Precedence diagrams######

##Absolute frequency
# ev %>%
#   precedence_matrix(type = "absolute") %>%
#   plot

##Relative
# ev %>%
#   precedence_matrix(type = "relative") %>%
#   plot

##Antecedent-wise Frequencies
# ev %>%
#   precedence_matrix(type = "relative_antecedent") %>%
#   plot

# #Consequent-wise Frequencies
# ev %>%
#   precedence_matrix(type = "relative_consequent") %>%
#   plot

###

######Dotted Chart######
# # Static
# ev %>%
#     dotted_chart(x = "absolute", y = "start")

# ev %>%
#   dotted_chart(x = "relative", y = "duration", color = "resource")

## idotted_chart


######TRACE EXPLORER######
# #Frequent traces
# ev %>%
#   trace_explorer(coverage = 0.4)
# 
# ##infrequent traces
# ev %>%
#   trace_explorer(type = "infrequent", coverage = 0.1)

######Dashboards######
# performance_dashboard:
# activity_dashboard:
# rework_dashboard: 
# resource_dashboard: