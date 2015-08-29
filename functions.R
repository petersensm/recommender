# functions

# check to see if required packages are installed, then load them all
check_packages <- function(packagelist) {
    # make a vector of packages whose names are not in the installed packages
    new.packages <- packagelist[!(packagelist %in% installed.packages()[,"Package"])]
    # if there is any length to this list, install those packages
    if(length(new.packages)) {install.packages(new.packages)}
    # now load the packages one by one - my contribution!
    for(x in packagelist) {
        library(x, character.only = TRUE)
    }
}

# get the catalog
# this is a verision ripped from source_url() in the devtools package 
# I would not ordinarily do this, but knitr and my typical way of downloading files are not compatable
# bonus (maybe) this puts the file in temp and removes it when done
# since you won't be running knitr, feel free to use the version below!
get_catalog <-function() {
    temp_file <- tempfile()
    on.exit(unlink(temp_file))
    fileUrl <- "http://labrosa.ee.columbia.edu/millionsong/sites/default/files/tasteprofile/taste_profile_usercat_120k.txt"
    request <- httr::GET(fileUrl)
    httr::stop_for_status(request)
    writeBin(httr::content(request, type = "raw"), temp_file)
    catalog <- read.table(temp_file,  
                      comment.char = "", colClasses = "character", stringsAsFactors = F,
                      fill = T, quote = NULL, sep = ">", strip.white = T)
    goodcatalog <- subset(catalog, nchar(V2) == 18)
    goodcatalog
}

get_catalogV1 <- function(){
    fileUrl <- "http://labrosa.ee.columbia.edu/millionsong/sites/default/files/tasteprofile/taste_profile_usercat_120k.txt"
    download.file(fileUrl, destfile = "taste_profile_usercat_120k.txt")
    catalog <- read.table("taste_profile_usercat_120k.txt",  
                          comment.char = "", colClasses = "character", stringsAsFactors = F,
                          fill = T, quote = NULL, sep = ">", strip.white = T)
    goodcatalog <- subset(catalog, nchar(V2) == 18)
    goodcatalog
}

# accessory functions for getprofiledata, 
# to get the same users as in the saved dataset, precede main function with set.seed(1) 

# generate list of catalog ids to pull
    getcatalogids <- function(catalog, recordnumber) {
        #set.seed(1)
        sample(catalog[["V2"]], recordnumber)
    }
    
# get url: input = catalog id; output = URL
    makeurl <- function(id) {
        source("api_key.R")
        URL <- paste("http://developer.echonest.com/api/v4/tasteprofile/read?api_key=",
                     apikey,
                     "&format=json&id=",
                     id, 
                     sep = "")
        URL 
    }
    
# get data fnx. input = URL; output data for single user
    getusercontent <- function(URL){
        usercontent <- fromJSON(content(GET(URL), as = "text"))
    }
    
# extract dataframe of playlist contents for a single user. given the text formated contents
    getuserdata <- function(contents){
        user <- contents
        usercatalog <- user$response$catalog$items[c("song_name","artist_id","artist_name","play_count","song_id")]
        usercatalog$user_id <- rep(user$response$catalog$id)
        usercatalog
    }

# main function for retrieving user tasteprofile data using the catalog index
get_profiledata <- function(catalog, recordnumber, ratelimit = 20) {
    # initialize dataframe
    result <- data.frame()
    # get list of catalog fnx - input = catalog, number of records
    catids <- getcatalogids(catalog, recordnumber)
    #print(catids)
    sleeptime <-  60/20
    # start loop
    for(id in catids){
        # pause for the number of seconds in sleeptime to not go over rate limit 
        Sys.sleep(sleeptime)
        # get url fnx - input = catalog id; output = URL
        URL <- makeurl(id)
        print(id)
        # get user content fnx - input = URL
        usercontent <- getusercontent(URL)
        # if the catalog data are good (i.e. have all 5 elements I want), 
        if(5 == length(intersect(names(usercontent$response$catalog$items), 
                                 c("song_name","artist_id","artist_name","play_count","song_id")))) {
            # get user catalog 
            usercat <- getuserdata(usercontent) 
            # append data frame to initial
            result <- rbind(result, usercat)
        }
        else {
            # otherwise do not get or append the data
            warning(paste(usercontent$response$catalog$id,"missing one or more catalog items"))  
        }
    }
    # final output: dataframe of user catalogs
    result   
}

# easy button for getting previously retrieved user tasteprofile data posted on github
# again, this version is based on source_url() in the devtools package 
# bonus (maybe) this puts the file in temp and removes it when done
# since you won't be running knitr, feel free to use the version below!
loadsaveddata <-function() {
    temp_file <- tempfile()
    on.exit(unlink(temp_file))
    fileUrl <- "https://raw.githubusercontent.com/petersensm/recommender/master/tasteprofile600users.csv"
    request <- httr::GET(fileUrl)
    httr::stop_for_status(request)
    writeBin(httr::content(request, type = "raw"), temp_file)
    tasteprofile <- read.csv(temp_file, stringsAsFactors = F)
    tasteprofile
}

loadsaveddataV1 <- function(){
    fileUrl <- "https://raw.githubusercontent.com/petersensm/recommender/master/tasteprofile600users.csv"
    download.file(fileUrl, destfile = "tasteprofile600users.csv")
    tasteprofile <- read.csv("tasteprofile600users.csv", stringsAsFactors = F)
    tasteprofile
}

# process user profile data for feeding into binary recommender
process_profiles <- function(profiledata) {
    profiledata <- profiledata
    # reduce profile dataframe to taste profile (user, item, play_count) tupels 
    tupels <- profiledata[c("user_id","song_id","play_count")]
    # convert NAs in play_count to values of 1  
    tupels$play_count[is.na(tupels$play_count)] <- 1
    # use as() from recommederlab to coherse the dataframe into a object of class realRatingMatrix
    ratingmatrix <- as(tupels, "realRatingMatrix")
    # and use binarize() from recommenderlab totransform to binary data (0/1 for absence/presence)
    binarymatrix <- binarize(ratingmatrix, minRating = 1)
    # return binaryRatingMatix object
    binarymatrix
}

# split_data function 
split_data <- function(binarymatrix, training_split = .9){
    n <- nrow(binarymatrix)
    # default training_split is .9 or 90%
    training_split <- training_split
    # calculate the number of users to pull for the training data set
    perc_training <- round(n*training_split)
    # pseudo randomly sample rowname indices equal to the percentage to go into training 
    training <- sample(1:n, perc_training)
    # return the row index for selecting training samples
    training
}

# accessory functions to go into the process_recommendations function

# retrieve the user id for a user in the training data using the index - which also corresponds with the topndata
    get_userid <- function(test_data, user_number){
        test_data
        user_id <- test_data@data@itemsetInfo[user_number,]
        user_id
    }
    
# pull the playlist (a dataframe of songs and info) for a particular user
    get_user_playlist <- function(profiledata, user_id){
        # use user_id to look subset for the user playlist of songs and astists
        playlist <- profiledata[profiledata$user_id == user_id,c("song_id", "song_name", "artist_name", "artist_id")]
        playlist
    }
    
# generate a dataframe of recommended songs + info for a particular user
    get_user_recs <- function(profiledata, prediction, user_number){
        # generate an index of songs from the profiledata (using select and distinct from package dply for the moment)
        # I do know that at least 6 of the song ids are duplicated (with diff artist info) so this could be a problem
        songindex <- distinct(select(profiledata, song_id, song_name, artist_name, artist_id )) 
        # retrieve list of recommended song_ids for the user
        topnlist <- as(prediction, "list")[[user_number]]
        # use song_id to look up song_name and artist info in the songindex
        topn_songs <- data.frame()
        for(song in topnlist){
            topsongi <- songindex[songindex$song_id == song,]
            topn_songs <- rbind(topn_songs, topsongi)
        }
        topn_songs
    }

# function for processing recommendations
# for each user in the test_data set, produce a list with their id, playlist, and their top 5 recommendations 
# output is a list the length of the number of users in test_data
process_recommendations <- function(profiledata, test_data, prediction) {
    n <- nrow(test_data)
    # initialize list
    result <- list()
    # a list for users
    for(i in 1:n){
        # id
        user_id <- get_userid(test_data, i)
        # dataframe of playlist
        user_playlist <- get_user_playlist(profiledata, user_id)
        # dataframes of songs recommended
        user_recommendations <- get_user_recs(profiledata, prediction, i)
        # for each user a list of 3
        user_list_i <- list(user_output = list(user_id = user_id, 
                                               playlist = user_playlist, 
                                               recommendations = user_recommendations))
        # glue each user's list onto the results list
        result <- c(result, user_list_i)
    }
    # output the results list
    result
}

# one combined function that takes the profile data, 
# completes the whole data pipeline from profiles through prediction, 
# and returns original playlists and top 5 (or n) recommendations for 10 % of the users
get_recommendations <- function(profiledata, method, training_split = .9,  n = 5) {
    # process user profile data for feeding into the binary recommender; involves using as and binarize from recommenderlab
    binarymatrix <- process_profiles(profiledata)
    # split the data into (by default) training (90%) and test (10%) data
    training_subset_index <- split_data(binarymatrix, training_split = training_split)
    # subset data into a training dataset based on user index numbers sampled for training
    train_data <- binarymatrix[training_subset_index]
    # subset data into a test dataset based on user index numbers not sampled for training
    test_data <- binarymatrix[-training_subset_index]
    # build one of the binary recommenders using the training data; using Recommender from recommenderlab
    if(method == "UBCF"){
        # call the User based collborative filtering method, and use the Jaccard, with 5 nearest neighbors
        rec <- Recommender(train_data, method = "UBCF", param= list(method = "Jaccard", nn = 5))
        print("Recommender using collaborative filtering with Jaccard's similarity and 5 nearest neighbors")
    } 
    if(method =="POPULAR") {
        # run the popular (vote counting) method
        rec <- Recommender(train_data, method = "POPULAR") 
        print("Recommender using the most popular songs (i.e. those occuring in the most playlists)")
    }
    # get predictions; using predict from recommenderlab; default is 5 recommendations
    predictions <- predict(rec, test_data, n = n)
    # get playlists and recommendations for users in the test data set
    output <- process_recommendations(profiledata, test_data, predictions)
    output
}