connecting R to a SQLite database:
- working with data larger than memory
- delay.con <- dbConnect("SQLite3", dbname="")
- dbConnect creates a connection to the database that we can use in subsequent commands
- install.packages("RSQLite")
- dbGetQuery(delay.con,"SELECT COUNT(*),Year FROM AirlineDelay WHERE Year=1987")
- dbGetQuery is an R function that sends an SQL query to the database and waits for the result

- inside the directory where your csv files reside, type sqlite3 AirlineDelay.sqlite3 to create a database named AirlineDelay
- now we can interact with the database in SQL and create a schema for our table: 
CREATE TABLE AirlineDelay (
    Year int,
    Month int,
    DayOfMonth int,
    DayOfWeek int,
    DepTime int,
    CRSDepTime int,
    ArrTime int,
    CRSArrTime int,
    UniqueCarrier varchar(5),
    FlightNum int,
    TailNum varchar(8),
    ActualElapsedTime int,
    CRSEElapsedTime int,
    AirTime int,
    ArrDelay int,
    DepDelay int,
    Origin varchar(3),
    Dest varchar(3),
    Distance int,
    TaxiIn int,
    TaxiOut int,
    Cancelled int,
    CancellationCode varchar(1),
    Diverted varchar(1),
    CarrierDelay int,
    WeatherDelay int,
    NASDelay int,
    SecurityDelay int,
    LateAircraftDelay int
);

- after we've created the above table, import the data: .import 1987.csv AirlineDelay

to interact with the data:
- dbGetQuery is an R function that sends an SQL query to the database to be processed and waits for the result
