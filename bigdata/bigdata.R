# importing the airline delay data into a SQLite database:
install.packages("RSQLite")
library(RSQLite)
delay.con <- dbConnect(SQLite(), dbname = "AirlineDelay.sqlite3")
delays87 <- dbGetQuery(delay.con, "SELECT * FROM AirlineDelay WHERE Year=1987")
nrows(delays87)
