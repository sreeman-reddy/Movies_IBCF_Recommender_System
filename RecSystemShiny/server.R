get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

myIBCF <- function(newuser) {
  
  S <- read.csv("Smat.csv", row.names = 1)
  popularity_ranking <- read.csv("popular50.csv")
  
  predict_rating <- function(i) {
    S_i <- S[i, ]
    valid_indices <- which(!is.na(newuser) & !is.na(S_i))
    if (length(valid_indices) == 0) return(NA)
    
    num_sum <- sum(S_i[valid_indices] * newuser[valid_indices])
    denom_sum <- sum(S_i[valid_indices])
    
    if (denom_sum == 0) return(NA)
    return(num_sum / denom_sum)
  }
  
  predictions <- sapply(1:length(newuser), function(i) {
    if (is.na(newuser[i])) predict_rating(i) else NA
  })
  
  names(predictions) <- colnames(rmat)
  
  top_indices <- order(predictions, decreasing = TRUE)[1:10]
  top_predictions <- predictions[top_indices]
  

  if (sum(!is.na(top_predictions)) < 10) {
    na_count <- sum(is.na(top_predictions))
    popular_indices <- popularity_ranking$MovieID[!(popularity_ranking$MovieID %in% which(!is.na(newuser)))]
    fill_indices <- popular_indices[1:na_count]
    #print(fill_indices)
    top_indices <- c(names(top_predictions[!is.na(top_predictions)]), fill_indices)
  }
  
  recommendations <- colnames(S[top_indices[1:10]])
  # print(recommendations)
  return(recommendations)
}


# read in data
rmat <- read.csv("Rmat.csv", row.names=1, header = TRUE)
myurl = "https://liangfgithub.github.io/MovieData/movies.dat?raw=true"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")
movies = movies[!(movies$MovieID %in% c(109, 115, 51)),]  #Excluding movieID 51 (Guardian Angel),
                                                          #115(Happines in the Field),
                                                          #109 (Headless Body in topless Bar) as they dont exist in ratings
small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

shinyServer(function(input, output, session) {
  
  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", movies$authors[(i - 1) * num_movies + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      #print(user_ratings) #movieID 51 (Guardian Angel), 115(Happines in the Field), 109 (Headless Body in topless Bar)causing issues
      newuser <- rep(NA, ncol(rmat))
      names(newuser) <- colnames(rmat)
      newuser[paste0("m",user_ratings$MovieID)] <- user_ratings$Rating
      # print(length(newuser))
      #user_results = (1:10)/10
      user_predicted_ids = as.integer(sub("m","",myIBCF(newuser)))
      #print(user_predicted_ids)
      filtered_movies <- movies[movies$MovieID %in% user_predicted_ids, ]
      ordered_movies <- filtered_movies[match(user_predicted_ids, filtered_movies$MovieID),]
      
      # recom_results <- data.table(Rank = 1:10, 
      #                             MovieID = movies$MovieID[user_predicted_ids], 
      #                             Title = movies$Title[user_predicted_ids]) 
                                  #Predicted_rating =  user_results)
      # recom_results <- data.table(Rank = 1:10, 
      #                             MovieID = movies[movies$MovieID %in% user_predicted_ids,'MovieID'], 
      #                             Title = movies[movies$MovieID %in% user_predicted_ids,'Title']) 
      recom_results <- data.table(Rank = 1:10,
                                  MovieID = ordered_movies$MovieID,
                                  Title = ordered_movies$Title)
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    #print(recom_result)
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                #a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
                a(img(src = movies[movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j],'image_url'], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                #strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
                strong(movies[movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j], 'Title'])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function
