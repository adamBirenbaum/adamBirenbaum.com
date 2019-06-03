
vars <- reactiveValues(games = list(),winner = NULL)

adj <<- c("Attractive", "Bald", "Beautiful", "Chubby", "Clean", "Dazzling", 
          "Drab", "Elegant", "Fancy", "Fit", "Flabby", "Glamorous", "Gorgeous", 
          "Handsome", "Long", "Magnificent", "Muscular", "Plain", "Plump", 
          "Quaint", "Scruffy", "Shapely", "Short", "Skinny", "Stocky", 
          "Ugly", "Unkempt", "Unsightly")

animal <<- c("Aardvark", "Aardwolf", "African Elephant", "African Tree Pangolin", 
             "Alligator", "Alpaca", "Anteater", "Antelope", "Ape", "Arabian Horse", 
             "Armadillo", "Arthropod", "Asian Elephant", "Aye-Aye", "Baboon", 
             "Badger", "Bandicoot", "Bangle Tiger", "Bat", "Bearded Dragon", 
             "Beaver", "Beluga Whale", "Bengal Tiger", "Big-Horned Sheep", 
             "Billy Goat", "Bird", "Bison", "Black Bear", "Black Footed Rhino", 
             "Black Howler Monkey", "Black Rhino", "Blacktip Reef Shark", 
             "Blue Shark", "Blue Whale", "Boar", "Bob-Cat", "Bonobo", "Bottlenose Dolphin", 
             "Bottlenose Whale", "Brown Bear", "Buffalo", "Bull", "Bull frog", 
             "Caiman Lizard", "Camel", "Capuchin Monkey", "Capybara", "Caribou", 
             "Cat", "Cattle", "Cheetah", "Chimpanzee", "Chinchilla", "Chipmunk", 
             "Common Dolphin", "Common Seal", "Coral", "Cougar", "Cow", "Coyote", 
             "Crocodile", "Crustacean", "Dart Frog", "Deer", "Degus", "Dik-Dik", 
             "Dingo", "Dog", "Dogfish Shark", "Dolphin", "Donkey", "Door Mouse", 
             "Dormouse", "Draft Horse", "Duckbill Platypus", "Dugong", "Dusky Shark", 
             "Elephant", "Elephant Seal", "Elk", "Ermine", "Eurasian Lynx", 
             "Ferret", "Fish", "Florida Panther", "Flying Fox", "Fox", "Fresh Water Crocodile", 
             "Frog", "Fur Seal", "Galapagos Land Iguana", "Galapagos Shark", 
             "Galapagos Tortoise", "Gazelle", "Gecko", "Giant Anteater", "Giant Panda", 
             "Gibbon", "Giraffe", "Goat", "Gopher", "Gorilla", "Gray Fox", 
             "Gray Nurse Shark", "Gray Reef Shark", "Gray Whale", "Great White Shark", 
             "Green Poison Dart Frog", "Green Sea Turtle", "Grizzly Bear", 
             "Groundhog", "Guinea Pig", "Hairy-Nosed Wombat", "Hammerhead Shark", 
             "Harbor Porpoise", "Harbor Seal", "Hare", "Harp Seal", "Hawaiian Monk Seal", 
             "Hedgehog", "Hippopotamus", "Horn Shark", "Horse", "Howler Monkey", 
             "Humpback Whale", "Hyena", "Hyrax", "Iguana", "Iguanodon", "Impala", 
             "Insect", "Irrawaddy Dolphin", "Jackal", "Jackrabbit", "Jaguar", 
             "Jellyfish", "Kangaroo", "Kermode Bear", "Killer Whale", "Koala", 
             "Komodo Dragon", "Kookaburra", "Lama", "Lamb", "Lancelet", "Least Weasel", 
             "Leatherback Sea Turtle", "Lemming", "Lemon Shark", "Lemur", 
             "Leopard", "Leopard Gecko", "Leopard Seal", "Leopard Shark", 
             "Lion", "Llama", "Loggerhead Turtles", "Lynx", "Mako Shark", 
             "Manatee", "Manta Ray", "Mantis", "Marbled Salamander", "Marmot", 
             "Marsupial", "Meerkat", "Megamouth Shark", "Melon-Headed Whale", 
             "Miniature Donkey", "Mink", "Minke Whale", "Mole", "Mole Salamander", 
             "Mollusk", "Mongoose", "Monitor Lizard", "Monk Seal", "Monkey", 
             "Moose", "Mountain Lion", "Mouse", "Mule", "Muskox", "Muskrat", 
             "Naked Mole Rat", "Narwhal", "Nautilus", "Newt", "Northern Right Whale", 
             "Nurse Shark", "Nutria", "Nyala", "Ocelot", "Octopus", "Okapi", 
             "Opossum", "Orangutan", "Orca", "Otter", "Ox", "Panda", "Panther", 
             "Pig", "Pilot Whale", "Pine Marten", "Platypus", "Polar Bear", 
             "Porcupine", "Porpoise", "Possum", "Potbellied Pig", "Potto", 
             "Prairie Dog", "Proboscis Monkey", "Przewalski's Wild horse", 
             "Puma", "Pygmy Hippopotamus", "Pygmy Right Whale", "Pygmy Sperm Whale", 
             "Quokkas", "Quolls", "Rabbit", "Raccoon", "Rat", "Ray", "Red Fox", 
             "Red Kangaroo", "Red Panda", "Red-Eyed Tree Frog", "Reef Shark", 
             "Reindeer", "Rhino", "Rhinoceros", "Right Whale", "Ringed Seal", 
             "Risso's Dolphin", "River Dolphin", "Salamander", "Sandbar Shark", 
             "Scalloped Hammerhead Shark", "Sea Lion", "Sea Turtles", "Seal", 
             "Sei Whale", "Shark", "Sheep", "Shortfin Mako Shark", "Siberian Tiger", 
             "Silky Shark", "Skink", "Skunk", "Slender Loris", "Sloth", "Sloth Bear", 
             "Snake", "Snow Fox", "Snow Hare", "Snow Leopard", "Snow Monkey", 
             "Somali Wild Ass", "Spectacled Bear", "Sperm Whale", "Spider Monkey", 
             "Spiny Dogfish Shark", "Spotted Dolphin", "Squirrel", "Star-Nosed Mole", 
             "Striped Dolphin", "Sun Bear", "Takin", "Tamarin", 
             "Tapir", "Tasmanian Devil", "Tasmanian Tiger", "Terrapin", "Thresher Shark", 
             "Tiger", "Tiger Salamander", "Tiger Shark", "Topi", "Tortoise", 
             "Tree Frog", "Turtle", "Uakari", "Vampire Bat", "Vancouver Island Marmot", 
             "Vervet Monkey", "Vicuna", "Vole", "Wallaby", "Walrus", "Warthog", 
             "Water Buffalo", "Weasel", "Whale Shark", "Whale", "White Rhino", 
             "White Tailed Deer", "White-Beaked Dolphin", "Whitetip Reef Shark", 
             "Wildcat", "Wildebeest", "Wobbegong Shark", "Wolf", "Wolverine", 
             "Wombat", "Woodchuck", "Yak", "Yellow-Bellied Marmot", "Zebra", 
             "Zebu", "Zorilla")

countries <<- c("Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua & Deps", 
                "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", 
                "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", 
                "Belize", "Benin", "Bhutan", "Bolivia", "Bosnia Herzegovina", 
                "Botswana", "Brazil", "Brunei", "Bulgaria", "Burkina", "Burundi", 
                "Cambodia", "Cameroon", "Canada", "Cape Verde", "Central African Rep", 
                "Chad", "Chile", "China", "Colombia", "Comoros", "Congo", "Congo", 
                "Costa Rica", "Croatia", "Cuba", "Cyprus", "Czech Republic", 
                "Denmark", "Djibouti", "Dominica", "Dominican Republic", "East Timor", 
                "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", 
                "Estonia", "Ethiopia", "Fiji", "Finland", "France", "Gabon", 
                "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Grenada", 
                "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", 
                "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", 
                "Israel", "Italy", "Ivory Coast", "Jamaica", "Japan", "Jordan", 
                "Kazakhstan", "Kenya", "Kiribati", "Korea North", "Korea South", 
                "Kosovo", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", 
                "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", 
                "Luxembourg", "Macedonia", "Madagascar", "Malawi", "Malaysia", 
                "Maldives", "Mali", "Malta", "Marshall Islands", "Mauritania", 
                "Mauritius", "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia", 
                "Montenegro", "Morocco", "Mozambique", "Myanmar", "Namibia", 
                "Nauru", "Nepal", "Netherlands", "New Zealand", "Nicaragua", 
                "Niger", "Nigeria", "Norway", "Oman", "Pakistan", "Palau", "Panama", 
                "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", 
                "Portugal", "Qatar", "Romania", "Russian Federation", "Rwanda", 
                "St Kitts & Nevis", "St Lucia", "Saint Vincent & the Grenadin", 
                "Samoa", "San Marino", "Sao Tome & Principe", "Saudi Arabia", 
                "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", 
                "Slovakia", "Slovenia", "Solomon Islands", "Somalia", "South Africa", 
                "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", "Swaziland", 
                "Sweden", "Switzerland", "Syria", "Taiwan", "Tajikistan", "Tanzania", 
                "Thailand", "Togo", "Tonga", "Trinidad & Tobago", "Tunisia", 
                "Turkey", "Turkmenistan", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", 
                "United Kingdom", "United States", "Uruguay", "Uzbekistan", "Vanuatu", 
                "Vatican City", "Venezuela", "Vietnam", "Yemen", "Zambia", "Zimbabwe")

all_categories <<- structure(c("9", "10", "11", "12", "13", "14", "15", "16", 
                               "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", 
                               "28", "29", "30", "32"), .Names = c("General Knowledge", 
                                                                   "Books", "Film", "Music", "Musicals and Theatres", "Television", 
                                                                   "Video Games", "Board Games", "Science and Nature", "Computers", 
                                                                   "Mathematics", "Mythology", "Sports", "Geography", "History", 
                                                                   "Politics", "Art", "Celebrities", "Animals", "Vehicles", "Comics", 
                                                                   "Gadgets", "Cartoon and Animations"))


difficulty <<- structure(c("easy", "medium", "hard"), .Names = c("Easy", "Medium", 
                                                                 "Hard"))


server <- function(input,output,session){
  
  session_vars <- reactiveValues(team = NULL,team_ready = F, game_ready = F, question= NULL)
  
  
  
  format_question <<- function(q){
    q <- gsub("&quot;","'",q,fixed = T)
    q <- gsub("&#039;","'",q,fixed = T)
    
    q
  }
  
  new_question <- function(){
    list(
      option_categories = sample(all_categories,size = 3, replace = F),
      option_difficulties = sample(difficulty,size = 3, replace = F),
      chosen_category = NULL,
      chosen_difficulty = NULL,
      type = NULL,
      question = NULL,
      correct_answer = NULL,
      incorrect_answer = NULL
      
    )
  }
  
  generate_question <- function(q,category, difficult){
    base_url <- "https://opentdb.com/api.php?amount=1"
    api_req <- try(GET(url = paste0(base_url,"&category=",category,"&difficulty=",difficult)))
    if (inherits(api_req,'try-error')){
      Sys.sleep(2)
      api_req <- try(GET(url = paste0(base_url,"&category=",category,"&difficulty=",difficult)))
      if (inherits(api_req,'try-error')) return(NULL)
    }
    api_result <- content(api_req)
    api_result <- api_result$results[[1]]
    q$question <- format_question(api_result$question)
    q$type <- api_result$type
    q$correct_answer <- api_result$correct_answer
    q$incorrect_answer <- unlist(api_result$incorrect_answers)
    q
  }
  
  format_question = function(q){
    q <- gsub("&quot;","'",q,fixed = T)
    q <- gsub("&#039;","'",q,fixed = T)
    q
  }
  
  Question <- setRefClass("Question",
                          fields = list(option_categories = "character",
                                        option_difficulties = "character",
                                        type = "character",
                                        question = "character",
                                        correct_answer = "character",
                                        incorrect_answer = "character"
                          ),
                          methods = list(
                            initialize = function(){
                              option_categories <<- sample(all_categories,size = 3, replace = F)
                              option_difficulties <<- ample(difficulty,size = 3, replace = F)
                            },
                            format_question = function(q){
                              q <- gsub("&quot;","'",q,fixed = T)
                              q <- gsub("&#039;","'",q,fixed = T)
                              q
                            },
                            
                            generate_question = function(category,difficult){
                              base_url <- "https://opentdb.com/api.php?amount=1"
                              api_req <- GET(url = paste0(base_url,"&category=",category,"&difficulty=",difficult))
                              api_result <- content(api_req)
                              api_result <- api_result$results[[1]]
                              question <<- format_question(api_result$question)
                              type <<- api_result$type
                              correct_answer <<- api_result$correct_answer
                              incorrect_answer <<- unlist(api_result$incorrect_answers)
                            }
                          )
                          
  )
  
  
  Game <- setRefClass("Game",
                      fields = list(
                        num_players_req = "numeric",
                        num_players_joined = "numeric",
                        teams = "character",
                        score = "list",
                        game_name = "character",
                        current_turn = "character",
                        start_game = "logical"
                        
                      ),
                      methods = list(
                        initialize = function(num_play){
                          num_players_req <<- num_play
                          game_name <<- paste0(sample(countries,size = 1),paste0(sample(0:9,3,replace = T),collapse = ""))
                          start_game <<- F
                        },
                        add_team = function(team_name){
                          teams <<- c(teams, team_name)
                          if (length(num_players_joined) == 0){
                            num_players_joined <<- 1
                            current_turn <<- team_name
                          }else num_players_joined <<- num_players_joined + 1
                          
                          if (num_players_req == num_players_joined) start_game <<- T
                        }
                        
                      )
  )
  new_game <- function(num_play){
    list(
      num_players_req = num_play,
      num_players_joined = 0,
      game_name = paste0(sample(countries,size = 1),paste0(sample(0:9,3,replace = T),collapse = "")),
      start_game = F,
      teams = character(0),
      score = numeric(0),
      current_turn = character(0),
      start_game = F,
      game_end = F
    )
  }
  
  new_team <- function(game,is_my_turn){
    list(
      game_name = game$game_name,
      team_name = paste0(sample(adj,size = 1)," ", sample(animal,1)),
      my_turn = is_my_turn
    )
  }
  
  add_team <- function(game,team){
    game$teams <- c(game$teams, team$team_name)
    
    if (game$num_players_joined == 0) game$current_turn <- team$team_name
    
    game$num_players_joined <- game$num_players_joined + 1
    if (game$num_players_joined == game$num_players_req){
      game$start_game <- T
      
    } 
    
    game
  }
  
  is_game_over <- function(){
    if (is.null(session_vars$team)) return(T)

    game_end <- vars$games[[session_vars$team$game_name]]$game_end
    if (is.null(game_end)) return(T)
    vars$games[[session_vars$team$game_name]]$game_end
  }
  
  Team <- setRefClass("Team",
                      fields = list(
                        game_name = "character",
                        team_name = "character",
                        my_turn = "logical"
                      ),
                      methods = list(
                        initialize = function(g, turn){
                          team_name <<- paste0(sample(adj,size = 1)," ", sample(animal,1))
                          game_name <<- g
                          my_turn <<- turn
                          
                          
                          
                        }
                      ))
  
  
  observe(
    output$ui_start <- renderUI({
      tagList(
        h1("Trivia!!"),
        primaryActionButton("new_game","New Game"),
              primaryActionButton("join_game","Join Game"))
    })
    
  )
  
  observeEvent(input$new_game,{
    output$ui_start <- renderUI(NULL)
    output$ui_join_game <- renderUI(NULL)
    output$ui_new_game <- renderUI({
      tagList(
        sliderInput("num_teams","Number of Teams",min = 2, max = 10,value = 2),
        sliderInput("num_win","Score to Win", min = 5, max = 100, value = 50),
        primaryActionButton("enter_team_num","Start Game")
      )
    })
  })
  
  
  #vars <- reactiveValues(index = NULL, games = NULL,num_teams_req = NULL, num_teams_joined = NULL, teams = NULL, score = NULL, start_game = NULL, team_turn = NULL)
  
  observeEvent(input$enter_team_num,{
    
    game <- new_game(input$num_teams)
    team <- new_team(game,is_my_turn = T)
    game <- add_team(game,team)
    
    
    
    isolate({
      vars$games[[game$game_name]] <- game
      session_vars$team <- team
      session_vars$team_ready <- T
      
    })
    
    
    output$ui_new_game <- renderUI(NULL)
    
    output$ui_wait_for_teams <- renderUI({
      tagList(
        h1(paste0("Game: ", game$game_name)),
        h2(paste0("Teams Ready: 1 / ",game$num_players_req))
      )
    })
  })
  
  observeEvent(input$join_game,{
    
    
    
    game_names <- names(vars$games)
    
    if (length(game_names) == 0){
      output$ui_join_game <- renderUI(h2("No games to join.  Click new game"))
    }else{
      output$ui_start <- renderUI(NULL)
      output$ui_new_game <- renderUI(NULL)
      output$ui_join_game <- renderUI({
        tagList(
          radioButtons("game_to_join","Choose a game to join", choices = names(vars$games)),
          primaryActionButton("join_enter","Enter")
        )
        
      })
    }
    
  })
  
  observeEvent(input$join_enter,{
    
    output$ui_join_game <- renderUI(NULL)
    game <- vars$games[[input$game_to_join]]
    team <- new_team(game,is_my_turn = F)
    
    
    isolate({
      vars$games[[input$game_to_join]] <- add_team(vars$games[[input$game_to_join]],team)
      
      session_vars$team <- team
      session_vars$team_ready <- T
    })
    
    
    
    
    output$ui_wait_for_teams <- renderUI({
      tagList(
        h1(paste0("Game: ", input$game_to_join)),
        h2(paste0("Teams Ready: ",vars$games[[input$game_to_join]]$num_players_joined," / ",vars$games[[input$game_to_join]]$num_players_req))
      )
    })
  })
  
  
  
  
  ## Observer for readying the game
  observe({
    vars$games
    output$ui_wait_for_teams <- renderUI({
      if (session_vars$team_ready){
        print("in here")
        tagList(
          h1(paste0("Game: ", session_vars$team$game_name)),
          h2(paste0("Teams Ready: ",vars$games[[session_vars$team$game_name]]$num_players_joined," / ",vars$games[[session_vars$team$game_name]]$num_players_req))
        )
      }
    })
    
  })
  
  get_scoreboard <- function(game){
    my_team <- session_vars$team$team_name
    game_teams <- game$teams
    ind <- game_teams == my_team
    tagList(h3("Scoreboard"),mapply(function(x,myteam){
      if(myteam) return(tagList(tags$span(style="color:red",x),br()))
      tagList(tags$span(style="color:black",x),br())
    }, paste0(game$teams," = ",game$score),ind,SIMPLIFY = F))
           # lapply(paste0(game$teams," = ",game$score),function(x) tagList(tags$span(style="color:red",x),br())))
  }
  ## Observer for starting the game
  observe({
    
    if (is.null(session_vars$team)){
      return(NULL)
    }else{
      start <- vars$games[[session_vars$team$game_name]]$start_game
    
      
      if (identical(start,T)){
        print("Start game")
        PTS_TO_WIN <- isolate(input$num_win)
        game <- isolate(vars$games[[session_vars$team$game_name]])
        
        if(any(game$score >= PTS_TO_WIN)){
          
          vars$games[[session_vars$team$game_name]]$game_end <- T
          
          
          
        }else{
          if (length(game$score) == 0){
            vars$games[[session_vars$team$game_name]]$score <- replicate(length(game$teams),0) # set scores to 0
          }
          
          
        }
        
        if (length(game$score) == 0){
          vars$games[[session_vars$team$game_name]]$score <- replicate(length(game$teams),0) # set scores to 0
        }else{
          output$ui_wait_for_teams <- renderUI(NULL)
          session_vars$game_ready <<- T
          output$ui_game_board <- renderUI({
            print(" printing scoreboard")
            tagList(
              get_scoreboard(game),
              hr(),
              h3(paste0("Up now: ", game$current_turn)),
              uiOutput("ui_category")
            )
            
          })
          
        }
        
        
        
        
        
      }
      
    }
    
  })
  
  create_question_buttons <- function(question){
    categories <- question$option_categories
    difficulties <- question$option_difficulties
    button_text <- paste0(names(categories), " - ", names(difficulties))
    button_id <- c("cat1","cat2","cat3")
    mapply(function(in_id,lab) successActionButton(in_id,lab), button_id,button_text,SIMPLIFY = F)
  }
  
  # adjust session turns
  observe({
    vars$games
    
    if (session_vars$game_ready){
      if (is_game_over()){
        return(NULL)
        current_team_turn <- vars$games[[session_vars$team$game_name]]$current_turn # returns True when game was reset (error)
        
      }else{
        current_team_turn <- tryCatch(vars$games[[session_vars$team$game_name]]$current_turn,error = function(e) T) # returns True when game was reset (error)
        if (current_team_turn == session_vars$team$team_name){
          session_vars$team$my_turn <- T
        }else  session_vars$team$my_turn <- F
        
      }
      

      
    }
    
    
    
    
    
  })
  
  observe({
    if (session_vars$game_ready){
      if (!is_game_over()){
        my_turn <- session_vars$team$my_turn
        if  (my_turn){
          output$ui_category <- renderUI({
            
            question <- new_question()
            session_vars$question <- question
            
            tagList(
              h4("Pick a category:"),
              create_question_buttons(question)
              
            )
            
          })
        }else{
          output$ui_category <- renderUI({
            h5("Waiting for team to finish...")
          })
        }
        
      }
    }
  })
  
  observeEvent(c(input$cat1,input$cat2,input$cat3),{
    
    
    button_vals <- sapply(c(input$cat1,input$cat2,input$cat3), as.numeric)
    
    if (all(button_vals == 0)){
      return(NULL)
    }else{
     
      # Clicked a category
      cat_ind <- which(button_vals == 1)
      q <- session_vars$question
      category <- unname(q$option_categories[cat_ind])
      difficulty <- unname(q$option_difficulties[cat_ind])
      q$chosen_category <- category
      q$chosen_difficulty <- difficulty
      
      q <- generate_question(q,category,difficulty)
      if (is.null(q)){
        output$ui_question <- renderUI(h3("Error connecting to api.  Possible internet outage"))
        return(NULL)
      } 
      
      
      output$ui_category <- renderUI(NULL)
      session_vars$question <- q
      new_question <- q$question
      type <- q$type
      correct <- q$correct_answer
      incorrect <- q$incorrect_answer
      #if (type == "multiple"){
      output$ui_question <- renderUI({
        tagList(
          h1(new_question),
          radioButtons("question_choice",label = "",choices = sample(c(correct,incorrect),length(incorrect) + 1)),
          dangerActionButton("question_guess","Submit")
        )
        
      })
      
      #$ }
      
      
    }
    
  })
  
  observeEvent(input$question_guess,{
    
    q <- session_vars$question
    user_answer <- input$question_choice
    actual_answer <- q$correct_answer
    
    team <- session_vars$team
    game <- vars$games[[team$game_name]]
    team_index <- which(game$teams == team$team_name)
    if (user_answer == actual_answer){
      pts <- switch(q$chosen_difficulty,
                    'easy' = 1,
                    'medium' = 3,
                    'hard' = 5
      )
      game$score[team_index] <- game$score[team_index] + pts
      shinyWidgets::sendSweetAlert(session = session, title = NULL,text = tags$span(tags$h2("Correct!")), type = "success")
    }else{
      shinyWidgets::sendSweetAlert(session = session, title = NULL,text = tags$span(tags$h2("Wrong!"),tags$br(), tags$h4(paste0("Correct answer was: ", actual_answer))), type = "error")
    }
    
    n <- game$num_players_req
    if (team_index == n){
      turn_index <- 1
    }else turn_index <- team_index + 1
    
    game$current_turn <- game$teams[turn_index]
    vars$games[[team$game_name]] <- game
    output$ui_question <- renderUI(NULL)
    
  })
  
  # end the game
  observe({
    if (session_vars$game_ready){
      if (is_game_over()){
        output$ui_wait_for_teams <- renderUI(NULL)
        output$ui_game_board <- renderUI(NULL)
        output$ui_start <- renderUI({
          tagList(
            h1("Trivia!!"),
            primaryActionButton("new_game","New Game"),
                  primaryActionButton("join_game","Join Game"))
        })
        #browser()
        if (length(isolate(vars$games[[session_vars$team$game_name]])) == 0){
          winner <- isolate(vars$winner)
        }else{
          game <- isolate(vars$games[[session_vars$team$game_name]])
          win_ind <- which(game$score >= PTS_TO_WIN)
          winner <- game$teams[win_ind]
          vars$winner <- winner
        }

        shinyWidgets::sendSweetAlert(session = session, title = NULL,text = tags$span(tags$br(),tags$h4(paste0(winner," Wins"))), type = "success")
        
        
        try(
          isolate(vars$games[[session_vars$team$game_name]] <- NULL)
        )
        
        session_vars <<- reactiveValues(team = NULL,team_ready = F, game_ready = F, question= NULL)
        
      }
    }
  })
  
}