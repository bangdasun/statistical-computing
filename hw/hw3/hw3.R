# i - a
nets1617 <- readLines("C:\\Users\\Bangda\\Desktop\\GR5206 Materials\\Hwk3\\NetsSchedule.html")
# number of lines in html file
length(nets1617) 

# i - b
# total number of characters
sum(nchar(nets1617))

# i - c
# max number of characters in a single line
max(nchar(nets1617)) 

# iv
date.info <- "[A-Z]{1}[a-z]{2}[[:punct:]]\\s[A-Z]{1}[a-z]{2}\\s[0-9]{1,2}" 
date.data <- grep(nets1617, pattern = date.info)
# the first game
date.data[1] 
date.data[length(date.data)] 

# v
# find the full match expression
date <- regmatches(nets1617, regexpr(date.info, nets1617)) 
head(date)

# vi
# hours comes first and then ':' and minutes, then comes the 'PM', since all games time will be in 'PM'
time.info <- "[0-9]{1}[:][0-9]{2}\\s[P][M]" 
time <- regmatches(nets1617, regexpr(time.info, nets1617))
head(time)

# vii
# first match '<li class="game-status">' and follows '@' or 'v'
home.info <- '[<][i-l]{2}\\s[a-z]{5}[=]\\"[a-z]{4}[-][a-z]{6}\\"[>][@|v]' 
home <- regmatches(nets1617, regexpr(home.info, nets1617))
head(home)
# substring the last character and if it is 'v' then it is true
home <- (substr(home, nchar(home[1]), nchar(home[1])) == "v")
# convert TRUE and FALSE to 1 and 0
home <- as.numeric(home); head(home)

# viii
# similariy we can find the character before the opponents' name, it is the website with opponents' name and there are 3 modes:
# 1) single word like 'Chicago', with website name 'chicago-bulls'
# 2) two words like 'Golden State', with website name 'golden-state'
# 3) Piladelphia: it's website name contains numbers '76'
opponent.info <- '/[a-z]*[-]*[a-z]+[-][6-7]*[a-z]+\"[>][A-Z]{1,2}\\s*[A-Z]*[a-z]*' 
opponent <- regmatches(nets1617, regexpr(opponent.info, nets1617))
head(opponent)
tail(opponent)
# check the number of rows and find that the first and last row is nets itself that should be removed
length(opponent)
opponent <- opponent[-1]
opponent <- opponent[-length(opponent)]
# substring the name of opponents from the previous string
opponent.info2 <- '[A-Z]{1,2}[a-z]*[-]*[6-7]*[a-z]*\\s*[A-Z]*[a-z]*'
opponent <- regmatches(opponent, regexpr(opponent.info2, opponent))
head(opponent)

# ix
net.data <- data.frame(Date = date, Time = time, Opponent = opponent, Home = home)
net.data[1:10,]
# we can see that this data match the first 10 games as seen from the web browser
