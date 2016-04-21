# In this Project, we will extract some data from the  MySQL database - flights and create a Neo4j database. # We will use the following tables from flights:

# Airlines
# Airports
# Flights (a sample of 1000 rows)
# planes

# It is assumed that you have MySQL installed on the local system and the flights database available in MySQL 
# 
# Here are the various node types we will create:
# 
# Airline
# Orig
# Dest
# Flight
# Plane
# AircraftType
#  
# The below are the relations that we will create
# 
# Flight OWNED_BY Airline
# Flight FLEW_FROM Orig
# Flight FLEW_TO Dest
# Flight BY_PLANE Plane
# Plane OF_TyPE AircraftType


library(RMySQL)
library(RNeo4j)

# Get data from MySQL

# In this section we connect to MySQL and create data frames from all the tables in the database.

mydb = dbConnect(MySQL(), user='root', password='root', dbname='flights', host='localhost')

airlines = dbGetQuery(conn = mydb, statement = "Select * from airlines where 1=1")

planes = dbGetQuery(conn = mydb, statement = "Select * from planes")

aircraft_types =  dbGetQuery(conn = mydb, statement = "Select distinct model, manufacturer, type, engines, seats, speed from planes")

airports = dbGetQuery(conn = mydb, statement = "Select * from airports")

flights = dbGetQuery(conn = mydb, statement = "Select * from flights")

# Create data structure in Neo4j
#
# In this section we connect to Neo4j and create the structure for the flights database. 
# 
# Note: In my installation, I am disabling authentication.

graph = startGraph("http://localhost:7474/db/data/")
clear(graph, input = FALSE)

# create the nodes for Airlines
for (i in 1:nrow(airlines)){
    createNode(graph, "Airline", carrier = airlines[i,1], name = airlines[i,2])
}
addConstraint(graph, "Airline", "carrier")

# Next we create the Airports. Again here there are duplicates for the airport with FAA = "BFT". We will remove this row from the data

airports <- subset(airports, airports$name != 'BFT County Airport')

# Now we can create the Airports nodes 

for (i in 1:nrow(airports)){
    createNode(graph, c("Orig", "Dest"), Faa = airports[i,1], name = airports[i,2], lat = airports[i,3], lon = airports[i,4], alt = airports[i,5], tz = airports[i,6], dst = airports[i,7])
message("Done: ", i)    
}

addConstraint(graph, "Orig", "Faa")
addConstraint(graph, "Dest", "Faa")


# Next we create the nodes for the planes / Aircraft Types and relation between Planes and Aircraft Types
for (i in 1:nrow(planes)){
    plane = getOrCreateNode(graph, "Plane", Tailnum = planes[i,1], Year = planes[i,2], Engine = planes[i,9])
    aircraft_Type = getOrCreateNode(graph, "AircraftType", Model = paste0(planes[i,5], "-", planes[i,4]), Type = planes[i,3], Engines = planes[i,6], Seats = planes[i,7], Speed = planes[i,8])
    createRel(plane, "IS_OF_TYPE", aircraft_Type)
message("Done: ", i)
}
addConstraint(graph, "Plane", "Tailnum")
addConstraint(graph, "AircraftType", "Model")

# Finally we load the flights data and create the necessary relations. We first create a sample of only 500 records (it will take a long time to load the entire flights table)
fl_smp <- flights[sample(nrow(flights), 500, replace = FALSE), ] 

addConstraint(graph, "Flight", "flight_id")

for (i in 1:nrow(fl_smp)) {
    
    flight = getOrCreateNode(graph, "Flight", flight_id = row.names(fl_smp[i,]), year = fl_smp[i,1],
    month  = fl_smp[i,2], day  = fl_smp[i,3], dep_time  = fl_smp[i,4], dep_delay  = fl_smp[i,5], arr_time  = fl_smp[i,6], arr_delay  = fl_smp[i,7], carrier  = fl_smp[i,8], tailnum  = fl_smp[i,9], flight  = fl_smp[i,10], origin  = fl_smp[i,11], dest  = fl_smp[i,12], air_time  = fl_smp[i,13], distance  = fl_smp[i,14], hour  = fl_smp[i,15], minute  = fl_smp[i,16])

    airline = getOrCreateNode(graph, "Airline", carrier = fl_smp[i,8])
    createRel(flight, "OWNED_BY", airline)
    
    orig = getOrCreateNode(graph, "Orig", Faa = fl_smp[i,11])
    createRel(flight, "FLEW_FROM", orig)
    
    dest = getOrCreateNode(graph, "Dest", Faa = fl_smp[i,11])
    createRel(flight, "FLEW_TO", dest)

    plane = getOrCreateNode(graph, "Plane", Tailnum = fl_smp[i,9])
    createRel(flight, "BY_PLANE", plane)
    
message("Done: ", i)
}

summary(graph)

# Lets run a few queries and see outputs

# Which are all the flights that originated from "EWR" in the month of december 

query <- "MATCH (n1:Flight {month:12})-[:FLEW_FROM]->(n2:Orig {Faa:'EWR'}) RETURN n1,n2 ;"
x1<-cypherToList(graph, query)

# Which are all the flights that were by plane with tailnum "N807UA" in the month of december 

query <- "MATCH (n1:Flight {month:12})-[:BY_PLANE]->(n2:Plane {Tailnum:'N807UA'})RETURN n1,n2 ;"
x1<-cypherToList(graph, query)


