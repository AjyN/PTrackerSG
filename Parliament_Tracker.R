## PART 1 ##

# Reading in the data and basic formatting
# Note: Change the read.csv path as appropriate to
# Your folder and the filename
Quan_Tracker<-
  read.csv("C:/Users/jakes/RStudio_Files/PTracker_14th_Parliament/Test_Consolidated_PQSQ.csv",
           stringsAsFactors = FALSE)
#Replace NAs with 0s for convenience
Quan_Tracker$PQ[is.na(Quan_Tracker$PQ)] <- 0
  Quan_Tracker$SQ[is.na(Quan_Tracker$SQ)] <- 0

  #Add a party column using Key_MP_Dict

Key_MP_Dict<-
read.csv("C:/Users/jakes/RStudio_Files/PTracker_14th_Parliament/Test_Key_MP_Dict.csv",
           stringsAsFactors = FALSE)

#Use a loop to add a party column
Party<-vector(mode="character",length=(length(Quan_Tracker$Member)))
for (i in 1:length(Party)){
  this_MP<-Quan_Tracker$Member[i]
  if (!isTRUE(this_MP %in% Key_MP_Dict$Member)){
    print ("ERROR in this Member, at this row:")
    print (this_MP)
    print (i)
  } else {
    their_party<-Key_MP_Dict$Party[Key_MP_Dict$Member==this_MP]
    Party[i]<-their_party
  }
}

## IMPORTANT !! LOOK AT ERROR MESSAGE ## 

## RESOLVE ANY ISSUES IN NAMES OR ROWS HERE, WILL MAKE LIFE EASY##

#For changing names#
View(Key_MP_Dict)
Quan_Tracker$Member["row_number_1"]<-"corrected_name_1"
Quan_Tracker$Member["row_number_2"]<-"corrected_name_2"

#For removing blank rows#
Quan_Tracker<-Quan_Tracker[-c("row_numbers"),]

## PLEASE RESOLVE ISSUES USING THE FORMAT ABOVE ^^ ,

## THEN, WE RUN THE 'FOR' LOOP AGAIN

for (i in 1:length(Party)){
  this_MP<-Quan_Tracker$Member[i]
  if (!isTRUE(this_MP %in% Key_MP_Dict$Member)){
    print ("ERROR in this Member, at this row:")
    print (this_MP)
    print (i)
  } else {
    their_party<-Key_MP_Dict$Party[Key_MP_Dict$Member==this_MP]
    Party[i]<-their_party
  }
}

Party
Quan_Tracker<-data.frame(cbind(Quan_Tracker[,1:3],
                               Party, Quan_Tracker[,4:7]))

## PART 2 ##

# Preliminary processed data points which 
# can be used to check the final processed data later

#Get PQ SQ totals
Total_PQ<-sum(Quan_Tracker$PQ)
Total_SQ<-sum(Quan_Tracker$SQ)
Total_PQ
Total_SQ

#PQ SQ totals by party
PQ_by_Party<-aggregate(PQ~Party, Quan_Tracker, sum)
PQ_by_Party
SQ_by_Party<-aggregate(SQ~Party, Quan_Tracker, sum)
SQ_by_Party

#PQ SQ totals by ministry

PQ_by_ministry<-aggregate(PQ~Ministry, data = Quan_Tracker, sum)
SQ_by_ministry<-aggregate(SQ~Ministry, Quan_Tracker, sum)

## PART 3 ##

#Loop for generating tables

#Get list of ministries
Ministries<-unique(Quan_Tracker$Ministry)

#Sort according to letter order of names
Ministries<- Ministries[sort.list(Ministries)]

#Create vectors for assignment loop
PAPpq<-vector("numeric", length(Ministries))
WPpq<-vector("numeric", length(Ministries))
NMPpq<-vector("numeric", length(Ministries))
PSPpq<-vector("numeric", length(Ministries))
PAPsq<-vector("numeric", length(Ministries))
WPsq<-vector("numeric", length(Ministries))
NMPsq<-vector("numeric", length(Ministries))
PSPsq<-vector("numeric", length(Ministries))

#Assignment loop
for (i in 1:nrow(Quan_Tracker)){
  ministry_row<-which(Ministries==Quan_Tracker$Ministry[i])
  number_of_pqs<-Quan_Tracker$PQ[i]
  number_of_sqs<-Quan_Tracker$SQ[i]
  if (Quan_Tracker$Party[i]=="PAP"){
    PAPpq[ministry_row]<-PAPpq[ministry_row]+(number_of_pqs)
    PAPsq[ministry_row]<-PAPsq[ministry_row]+(number_of_sqs)
  } else if (Quan_Tracker$Party[i]=="WP"){
    WPpq[ministry_row]<-WPpq[ministry_row]+(number_of_pqs)
    WPsq[ministry_row]<-WPsq[ministry_row]+(number_of_sqs)
  } else if (Quan_Tracker$Party[i]=="NMP"){
    NMPpq[ministry_row]<-NMPpq[ministry_row]+(number_of_pqs)
    NMPsq[ministry_row]<-NMPsq[ministry_row]+(number_of_sqs)
  } else if (Quan_Tracker$Party[i]=="PSP"){
    PSPpq[ministry_row]<-PSPpq[ministry_row]+(number_of_pqs)
    PSPsq[ministry_row]<-PSPsq[ministry_row]+(number_of_sqs)
  } 
}

#Totals for PQs and SQs per ministry
PQtotal<-vector("numeric",length(Ministries))
for (i in 1:length(Ministries)){
  PQtotal[i]<-(PAPpq[i]+WPpq[i]+
                 NMPpq[i]+PSPpq[i])
}

SQtotal<-vector("numeric", length(Ministries))
for (i in 1:length(Ministries)){
  SQtotal[i]<-(PAPsq[i]+WPsq[i]+
                 NMPsq[i]+PSPsq[i])
}




## PART 4 ##

#Compiling of data frames and getting summary stat dataframe

PQ_output_data<-data.frame(Ministries,PAPpq,WPpq,
                           NMPpq,PSPpq,PQtotal)
SQ_output_data<-data.frame(Ministries,PAPsq,WPsq,
                           NMPsq,PSPsq,SQtotal)

View(PQ_output_data)
View(SQ_output_data)


## Part 5 ##

#Checks against earlier preliminary data, 
#Print data and export to CSV

#Checks PQ data table
for (i in 1:length(Ministries)){
  if (!isTRUE(PQ_output_data$PQtotal[i] %in% PQ_by_ministry$PQ [i])){
    print("WARNING, there is an error in")
    print(Ministries[i])
  } else {
    print ("All good")
  }
}

#Checks SQ data table
for (i in 1:length(Ministries)){
  if (!isTRUE(SQ_output_data$SQtotal[i] %in% SQ_by_ministry$SQ [i])){
    print("WARNING, there is an error in")
    print(Ministries[i])
  } else {
    print ("All good")
  }
}

#Now that it's checked, extract summary statistics
Questions_summary<-
  data.frame(cbind(c("Summary Data","--","--","--","--","--"),
                   c("No. of PQs",sum(PAPpq),sum(WPpq),sum(NMPpq),sum(PSPpq),sum(PQtotal))),
             c("No. of SQs",sum(PAPsq),sum(WPsq),sum(NMPsq),sum(PSPsq),sum(SQtotal))
  )
Questions_summary<-data.frame(t(Questions_summary))
rownames(Questions_summary)<-c(1:nrow(Questions_summary))

#Next, align column names of all data
#And add row of labels to separate PQ, SQ, summary

colnames(Questions_summary)=c("Ministry",
                              "PAP",
                              "WP",
                              "NMP",
                              "PSP",
                              "Total")

colnames(PQ_output_data)=c("Ministry",
                           "PAP",
                           "WP",
                           "NMP",
                           "PSP",
                           "Total")


PQ_identifier<-c("PQ","--","--","--","--","--")
PQ_identifier
PQ_output_data<-data.frame(rbind(PQ_identifier, 
                                 PQ_output_data))

colnames(SQ_output_data)=c("Ministry",
                           "PAP",
                           "WP",
                           "NMP",
                           "PSP",
                           "Total")
SQ_identifier<-c("SQ","--","--","--","--","--")
SQ_output_data<-data.frame(rbind(SQ_identifier, 
                                 SQ_output_data))

#Combine PQs, SQs, and Summary dataframes, export

Questions_by_party_ministry<-data.frame(
  rbind(Questions_summary,
        PQ_output_data,
        SQ_output_data))
View(Questions_by_party_ministry)
write.csv(Questions_by_party_ministry,
          "Questions_by_party_ministry.csv")

## PART 6 ##

#PQs and SQs by member

#PQs
PQ_by_member<-aggregate(PQ~Member, Quan_Tracker, sum)
PQ_by_member

#Similarly, use a loop to add Party column

Party_pqmember<-vector(mode="character",length=(length(PQ_by_member$Member)))
for (i in 1:length(Party_pqmember)){
  this_MP<-PQ_by_member$Member[i]
  if (!isTRUE(this_MP %in% Key_MP_Dict$Member)){
    print ("ERROR in this Member, at this row:")
    print (this_MP)
    print (i)
  } else {
    their_party<-Key_MP_Dict$Party[Key_MP_Dict$Member==this_MP]
    Party_pqmember[i]<-their_party
  }
}

#Order according to number of questions, combine into data frame
PQ_member_order<-sort.list(PQ_by_member$PQ,decreasing=TRUE)
PQ_by_member<-data.frame(PQ_by_member$Member[PQ_member_order],
                         Party_pqmember[PQ_member_order],
                         PQ_by_member$PQ[PQ_member_order])
colnames(PQ_by_member) = c("Member","Party", "No. of Questions")

#Add label for PQ section
PQ_by_member<-data.frame(rbind(c("PQs","--","--"),
                               PQ_by_member))

#SQs
SQ_by_member<-aggregate(SQ~Member, Quan_Tracker, sum)

#Similarly, use a loop to add Party column

Party_sqmember<-vector("character",length(SQ_by_member$Member))
for (i in 1:length(Party_sqmember)){
  this_MP<-SQ_by_member$Member[i]
  if (!isTRUE(this_MP %in% Key_MP_Dict$Member)){
    print ("ERROR in this Member, at this row:")
    print (this_MP)
    print (i)
  } else {
    their_party<-Key_MP_Dict$Party[Key_MP_Dict$Member==this_MP]
    Party_sqmember[i]<-their_party
  }
}

#Order and append into dataframe

SQ_member_order<-sort.list(SQ_by_member$SQ,decreasing=TRUE)
SQ_by_member<-data.frame(SQ_by_member$Member[SQ_member_order],
                         Party_sqmember[SQ_member_order],
                         SQ_by_member$SQ[SQ_member_order])
colnames(SQ_by_member) = c("Member","Party", "No. of Questions")

#Add label for SQ section
SQ_by_member<-data.frame(rbind(c("SQs","--","--"),
                               SQ_by_member))



#Combine and export

Questions_by_member<-data.frame(rbind(PQ_by_member,
                                      SQ_by_member))
View(Questions_by_member)
write.csv(Questions_by_member, 
          "Questions_by_member.csv")
=