library(httr)
library(dplyr)



categories <- list('General Knowledge' = "General Knowledge",
                   Entertainment = c("Books","Film","Music","Musicals & Theatres", "Television", "Video Games", "Board Games", "Cartoon & Animations"),
                   Science)

url <- read.table("https://opentdb.com/api_config.php")


cat_options <- '<option value="any">Any Category</option>
			<option value="9">General Knowledge</option><option value="10">Entertainment: Books</option><option value="11">Entertainment: Film</option><option value="12">Entertainment: Music</option><option value="13">Entertainment: Musicals &amp; Theatres</option><option value="14">Entertainment: Television</option><option value="15">Entertainment: Video Games</option><option value="16">Entertainment: Board Games</option><option value="17">Science &amp; Nature</option><option value="18">Science: Computers</option><option value="19">Science: Mathematics</option><option value="20">Mythology</option><option value="21">Sports</option><option value="22">Geography</option><option value="23">History</option><option value="24">Politics</option><option value="25">Art</option><option value="26">Celebrities</option><option value="27">Animals</option><option value="28">Vehicles</option><option value="29">Entertainment: Comics</option><option value="30">Science: Gadgets</option><option value="31">Entertainment: Japanese Anime &amp; Manga</option><option value="32">Entertainment: Cartoon &amp; Animations</option>'
  
cat_options <- strsplit(cat_options,'</option')[[1]]
cat_names <- gsub('.*>([:&;A-Za-z ]+)$',"\\1",cat_options)
cat_names <- cat_names[-26]
cat_num <- c("any",9:32)
cat_names <- gsub("&amp;","and",cat_names,fixed = T)
cat_names <- gsub("[a-zA-Z]+: (.*$)","\\1",cat_names)
cat_names <- cat_names[-24]
cat_num <- cat_num[-24]

categories <- cat_num
names(categories) <- cat_names
all_categories <- structure(c("any", "9", "10", "11", "12", "13", "14", "15", "16", 
                          "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", 
                          "28", "29", "30", "32"), .Names = c("Any Category", "General Knowledge", 
                                                              "Books", "Film", "Music", "Musicals and Theatres", "Television", 
                                                              "Video Games", "Board Games", "Science and Nature", "Computers", 
                                                              "Mathematics", "Mythology", "Sports", "Geography", "History", 
                                                              "Politics", "Art", "Celebrities", "Animals", "Vehicles", "Comics", 
                                                              "Gadgets", "Cartoon and Animations"))


difficulty <- structure(c("easy", "medium", "hard"), .Names = c("Easy", "Medium", 
                                                                "Hard"))


format_question <- function(q){
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
                            option_difficulties <<- sample(difficulty,size = 3, replace = F)
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
Team <- setRefClass("Team",
                    contains = "Game",
                    fields = list(
                      team_name = "character",
                      my_turn = "logical"
                    ),
                    methods = list(
                      initialize = function(){
                        team_name <<- paste0(sample(adj,size = 1)," ", sample(animal,1))
                      }
                    ))
                    
Game <- setRefClass("Game",
                    fields = list(
                      num_players = "numeric",
                      teams = "character",
                      score = "list",
                      game_name = "character"
                    ),
                    methods = list(
                      initialize = function(num_play){
                        num_players <<- num_play
                        game_name <<- paste0(sample(countries,size = 1),paste0(sample(0:9,3,replace = T),collapse = ""))
                      }
                    )
                    )

a <- Question$new()



cats <- a$option_categories
diffs <- a$option_difficulties
a$generate_question(cats[1],diffs[1])
a

adj <- c("attractive", "bald", "beautiful", "chubby", "clean", "dazzling", 
         "drab", "elegant", "fancy", "fit", "flabby", "glamorous", "gorgeous", 
         "handsome", "long", "magnificent", "muscular", "plain", "plump", 
         "quaint", "scruffy", "shapely", "short", "skinny", "stocky", 
         "ugly", "unkempt", "unsightly")

animal <- c("Aardvark", "Aardwolf", "African Elephant", "African Tree Pangolin", 
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

countries <- c("Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua & Deps", 
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
