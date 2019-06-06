# -*- coding: utf-8 -*-
"""
Created on Thu May 23 15:40:05 2019

@author: A-G-R
"""

###############################################################################
#Plugins
###############################################################################
# This plugin is needed for translating numbers to months
import datetime
# This plugin is needed for connecting with the database
import psycopg2

# This file contains the password for PgAdmin
import password

###############################################################################
#Connecting to pgAdmin
###############################################################################
conn = psycopg2.connect("dbname='Airbnber' \
                         host='localhost' \
                         user='postgres' \
                         password='"+password.kode+"'")

cur = conn.cursor()



###############################################################################
#Variables
###############################################################################

Buffers = ["250", "500", "750", "1000", "1250"]
Roomtypes = ["'Private room'", "'Entire home/apt'"]
ColName = ["NextYear","NextYear2"]
Criteria =["where hostlistin > 1 and activemont > 5","where hostlistin > 1", "where activemont > 5"]
CritNames =["RAH","RH","RA"]

###############################################################################
#Functions - Airbnb
###############################################################################
##Loop going through each month of rental sales - used for SwriteTheSQL
def SmonthsBeforeRent(RYear, RMonth):
    allActivityColumns = ""
    for x in range(1,12):
        #This if-statement makes sure that we don't end up with a date like 3/0/2017, by changing the year if that is the case
        if RMonth - x > 0: 
            monthinteger = RMonth - x
            year = datetime.date(RYear, monthinteger, 1).strftime('%y')
            month = datetime.date(RYear, monthinteger, 1).strftime('%b')
            allActivityColumns += "a" + str(month) + str(year) + " + "
        else:
            monthinteger = RMonth + 12 - x
            year = datetime.date(RYear - 1, monthinteger, 1).strftime('%y')
            month = datetime.date(RYear, monthinteger, 1).strftime('%b')
            allActivityColumns += "a" + str(month) + str(year) + " + "
    return allActivityColumns






#This function calculates the airbnb activity
def SwriteTheSQL(RentalYear, RentalMonth, Buffersize, RoomType):
   
    #Database source table name:
    AirbnbName = "airbnbpawel"
    
    #Database destination table name:
    BufferName = "b" + Buffersize
    
    # This shows names the column based on the room type
    if RoomType == "'Private room'":
        ColumnName = "rm2r" + Buffersize
    elif RoomType == "'Entire home/apt'":
        ColumnName = "rm2e" + Buffersize
    
    RequestString = ""
    RequestString += "update " + BufferName
    RequestString += " set " + ColumnName + " = sum "
    RequestString += "from ("
    RequestString += "select dwellingId, sum("
    
    #Referes to all airbnb data from months up to a year back 
    RequestString += SmonthsBeforeRent(RentalYear, RentalMonth) 
    #Referes to airbnb data from the same month (as the rental was sold) the difference between this and the other is that this doesnt include a " + " in the end (since it is the last value)
    RequestString += "a" + str(datetime.date(RentalYear, RentalMonth, 1).strftime('%b')) + str(datetime.date(RentalYear, RentalMonth, 1).strftime('%y')) + ") "
    RequestString += "from (select " + AirbnbName + ".*, airbnbupdated.roomtype from " + AirbnbName + " inner join airbnbupdated on airbnbupdated.room_id = " + AirbnbName + ".room_id) as ab,"
    RequestString += "(select * from " + BufferName + " where year = " + str(RentalYear)+ " and month = " + str(RentalMonth) +") as buf "
    RequestString += "where st_intersects(ab.geom, buf.geom) and ab.roomtype = " + RoomType +" "
    RequestString += "group by dwellingId) as counted "
    #This merges the datasets based on the dwellingId
    RequestString += "where " + BufferName + ".dwellingId = counted.dwellingId;"
    
    #Sends the command to the database if possible
    try:
        cur.execute(RequestString)
        conn.commit()
        print(str(RentalYear) + " " + str(RentalMonth) + " " + Buffersize + " " + RoomType + ": Works")
    #Otherwise returns the error message
    except psycopg2.Error as e: 
        print(e.pgerror)
        print(e.diag.severity)
        
#This does the same as the previous just with option to also have additional criteria
def AirbnbBuffedWithCrit(RentalYear, RentalMonth, Buffersize, RoomType, Crit, CritName):
    
    AirbnbName = "airbnbpawel"
    BufferName = "b" + Buffersize
    

    if RoomType == "'Private room'":
        ColumnName = "rm2r"
    elif RoomType == "'Entire home/apt'":
        ColumnName = "rm2e"
    
    FinalName = ColumnName + CritName
    RequestString = ""
    RequestString += "ALTER TABLE b"+Buffersize
    RequestString += " ADD COLUMN IF NOT EXISTS " + FinalName + " numeric(10,3);";

    RequestString += "update " + BufferName
    RequestString += " set " + FinalName + " = sum "
    RequestString += "from ("
    RequestString += "select dwellingId, sum("
    
    #Referes to all airbnb data from months up to a year back 
    RequestString += SmonthsBeforeRent(RentalYear, RentalMonth) 
    #Referes to airbnb data from the same month (as the rental was sold) the difference between this and the other is that this doesnt include a " + " in the end (since it is the last value)
    RequestString += "a" + str(datetime.date(RentalYear, RentalMonth, 1).strftime('%b')) + str(datetime.date(RentalYear, RentalMonth, 1).strftime('%y')) + ") "
    RequestString += "from (select " + AirbnbName + ".*, airbnbupdated.roomtype, airbnbupdated.hostlistin, airbnbupdated.activemont, airbnbupdated.reviewmax from " + AirbnbName
    RequestString += " inner join airbnbupdated on airbnbupdated.room_id = " + AirbnbName + ".room_id " + Crit + ") as ab,"
    RequestString += "(select * from " + BufferName + " where year = " + str(RentalYear)+ " and month = " + str(RentalMonth) +") as buf "
    RequestString += "where st_intersects(ab.geom, buf.geom) and ab.roomtype = " + RoomType +" "
    RequestString += "group by dwellingId) as counted "
    RequestString += "where " + BufferName + ".dwellingId = counted.dwellingId;"
    try:
        cur.execute(RequestString)
        conn.commit()
        print(str(RentalYear) + " " + str(RentalMonth) + " " + Buffersize + " " + RoomType + ": Works")
    except psycopg2.Error as e: 
        print(e.pgerror)
        print(e.diag.severity)


###############################################################################
#Functions - Rentals
###############################################################################


#This function calculates the average rental price/m2 in each buffer for each month - aslo counts the number of offers within the buffer at that point in time
def RentAVGinBuffers(RentalYear, RentalMonth, Buffersize):
    #Defines which table to change and add columns to store the data in
    RequestString = ""
    RequestString += "ALTER TABLE b"+Buffersize
    RequestString += " ADD COLUMN IF NOT EXISTS " + str(datetime.date(RentalYear, RentalMonth, 1).strftime('%b')) + "ravg" + str(datetime.date(RentalYear, RentalMonth, 1).strftime('%y')) + " numeric(10,3),";
    RequestString += " ADD COLUMN IF NOT EXISTS " + str(datetime.date(RentalYear, RentalMonth, 1).strftime('%b')) + "count" + str(datetime.date(RentalYear, RentalMonth, 1).strftime('%y')) + " bigint;";
    
    					
    RequestString += " update b" + Buffersize
    #Set the column to being the average of the rental price in the buffer
    RequestString += " set " +str(datetime.date(RentalYear, RentalMonth, 1).strftime('%b'))+"ravg" + str(datetime.date(RentalYear, RentalMonth, 1).strftime('%y'))+ " = avg, "
    #Count the number of offers in the buffer
    RequestString += str(datetime.date(RentalYear, RentalMonth, 1).strftime('%b'))+"count" + str(datetime.date(RentalYear, RentalMonth, 1).strftime('%y'))+ " = count"
    #Make the selection where it should search for rentals in
    RequestString += " from(select allcircles.dwellingid, avg(points.rentm2), count(allcircles.dwellingid)"
    #It should search for all points "points" from the given year and month within the buffer "allcircles", where the points intersects with the buffers
    RequestString += " from b" + Buffersize + " as allcircles, (select * from b" +Buffersize+" where year = " + str(RentalYear)+" and month = "+ str(RentalMonth) + ") as points"
    RequestString += " where St_intersects(allcircles.geom,st_centroid(points.geom))"
    RequestString += " group by allcircles.dwellingid) as avged"
    RequestString += " where avged.dwellingid = b"+Buffersize+".dwellingid;"
    try:
        cur.execute(RequestString)
        conn.commit()
        print(str(RentalYear) + " " + str(RentalMonth) + " " + Buffersize + ": Works")
    except psycopg2.Error as e: 
        print(e.pgerror)
        print(e.diag.severity)

def RentFromWhichMonth(RYear, RMonth):
     #This if-statement makes sure that we don't end up with a date like 3/13/2017, by changing the year if that is the case
    allActivityColumns = ""
    for x in range(1,12):
        if RMonth + x <= 12: 
            monthinteger = RMonth + x
            year = datetime.date(RYear, monthinteger, 1).strftime('%y')
            month = datetime.date(RYear, monthinteger, 1).strftime('%b')
            allActivityColumns += "(" + str(month) + "ravg" + str(year) + "), "
            
        else:
            monthinteger = RMonth - 12 + x
            year = datetime.date(RYear + 1, monthinteger, 1).strftime('%y')
            month = datetime.date(RYear, monthinteger, 1).strftime('%b')
            allActivityColumns += "(" + str(month) + "ravg" + str(year) + "), "
     
    return allActivityColumns

#Combines all the monthly values found above in an yearly average  
def RentAVGYearlyinBuffers(RentalYear, RentalMonth, Buffersize, ColumnName):
    #Defines the next year as the current year +1
    if ColumnName == "NextYear":
        SearchYear = RentalYear
    elif ColumnName == "NextYear2":
        SearchYear = RentalYear + 1
        
    RequestString = ""
    RequestString += "ALTER TABLE b"+Buffersize
    RequestString += " ADD COLUMN IF NOT EXISTS " + ColumnName + " numeric(10,3);";
    RequestString += " update b" + Buffersize
    RequestString += " set " + ColumnName + " = avgrent "    
    
    #Selects all month within 1 year of the when the rental was sold - all the buffers we get data from
    RequestString += " from (SELECT dwellingid, (SELECT AVG(c) FROM   (VALUES"
    RequestString += RentFromWhichMonth(SearchYear, RentalMonth)
    RequestString += "(" + str(datetime.date(SearchYear, RentalMonth, 1).strftime('%b'))+"ravg" + str(datetime.date(SearchYear, RentalMonth, 1).strftime('%y')) + ")) T (c)) AS avgrent"

    #This defines which buffers the data should be saves in - otherwise it will just overwrite itself - all the buffers we save data into
    RequestString += " From b" + Buffersize + " where year = " + str(RentalYear) + " and month = " + str(RentalMonth) + ") as link"    
    
    RequestString += " where link.dwellingid = b"+Buffersize+".dwellingid;"    

    try:
        cur.execute(RequestString)
        conn.commit()
        print(str(RentalYear) + " " + str(RentalMonth) + " " + Buffersize + " " +ColumnName + ": Works")
    except psycopg2.Error as e: 
        print(e.pgerror)
        print(e.diag.severity)
        

###########################################################################
#Loops        
###########################################################################    


#Aggregate the monthly averages rent/m2 into year ones:    
for col in ColName:
    for year in range(2015, 2019):
        for month in range(1,13):
            for buff in Buffers:
                RentAVGYearlyinBuffers(year, month, buff, col)


#This is the code for calculating average Rents
for buff in Buffers:
    for year in range(2015, 2018):
        for month in range(1,13):
            RentAVGinBuffers(year, month, buff)




#This is the code for calculating Airbnb activity
for buff in Buffers:
    for room in Roomtypes:
        for year in range(2016, 2019):
            for month in range(1,13):
                SwriteTheSQL(year, month, buff, room)




#This calculates the airbnb activity with aditional criterias
for buff in Buffers:
    for room in Roomtypes:
        for year in range(2016, 2019):
            for month in range(1,13):
                for crit in range(0,3):
                    AirbnbBuffedWithCrit(year, month, buff, room, Criteria[crit], CritNames[crit])



################################################################################
#This ends the connection to pgAdmin
################################################################################

if(conn):
    cur.close()
    conn.close()
