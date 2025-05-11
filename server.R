server <- function(input, output){
  
  
  
  model <- reactiveVal(luz_load("Sentimenter_goEmotions_e75.pt"))
  dictionary <- reactiveVal(readDictionary("goEmotions_embeddings_1_e75.csv"))
  
  ########################          Functions       ##############################
  ################################################################################
  ################################################################################
  #                           Clean Text Input ----
  
  # Splits into tokens, removes links hashtags and tags
  cleanInput <- function(x){
    
    x <- x %>%
      tolower() %>%
      str_remove_all("[.:!,?<>]") %>%
      str_split(" ")
    x <- lapply(x, function(y){y[!str_starts(y, "#")]})
    x <- lapply(x, function(y){y[!str_starts(y, "http")]})
    x <- lapply(x, function(y){y[!str_starts(y, "www")]})
    x <- lapply(x, function(y){y[!str_starts(y, "@")]})
    
    return(x)
    
  }
  
  
  ################################################################################
  ################################################################################
  #                           read dictionary ----
  
  readDictionary <- function(file){
    
    d <- read.csv(file)
    d <- d %>%
      select(all_of(starts_with("e")), token, id )
    
    return(d)  
    
  }
  
  
  ################################################################################
  ################################################################################
  
  #                           Token substitution ----
  
  #Substitues tokens for their matching vector from a given dictionary
  #Takes direct output from cleanInput
  tokenSubstitution <- function(x, dictionary, features, sentenceLength){
    
    x <- x[[1]]
    
    r <- list()
    
    j <- 1
    
    for(i in 1:length(x)){
      
      if(x[[i]] %in% dictionary$token){
        
        emb <- dictionary[which(dictionary$token == x[i]), ][1:features]
        
        r[[j]] <- torch_tensor(unlist(emb), dtype = torch_float())
        
        j <- j+1
      }
      
    }
    
    padding <- dictionary[which(dictionary$token == "<PAD>"), ][1:features]
    padding <- torch_tensor(unlist(padding), dtype = torch_float())
    
    for(i in (length(r) + 1):sentenceLength){
      
      r[[i]] <- padding
      
    }
    
    r <- torch_stack(r, dim = 1)
    
    return(r$unsqueeze(1))
    
  }
  
  ################################################################################
  ################################################################################
  #                           Padding Token  ----
  #                           
  getPaddingToken <- function(dictionary, features){
    
    padding <- dictionary[which(dictionary$token == "<PAD>"), ][1:features]
    
    padding <- data.frame(padding)
    
    return(padding)
    
  }
  
  
  ################################################################################
  ################################################################################
  #                           Preditcion wrapper ----
  
  predictSentiment <- function(
    model, text, dictionary, features = 50, sentenceLength = 50){
    
    model$model$paddings <- getPaddingToken(dictionary, features)
    
    p <- predict(
      model,
      tokenSubstitution(
        cleanInput(text),
        dictionary,
        features,
        sentenceLength
      )
    )
    p <- nnf_softmax(p, dim = 2)
    
    return(p)
    
  }
  
  
  ################################################################################
  ################################################################################
  
  
  
  output$pred <- renderPlot({
    
    p <- predictSentiment(model(), input$text, dictionary(), 75, 50)
    
    p <- c(as.matrix(p))
    
    names(p) <- c("Sadness", "Happiness", "Neutral", "Anger", "Anxiety")
    
    p <- data.frame(p)
    
    p$class <- rownames(p)
    
    ggplot(data = p, aes(x = class, y = p, fill = class)) + 
      geom_col(width = 0.90) + 
      geom_label(aes(x = class, y = p, label = round(p, 4) * 100), vjust = 0.01, size = 6) + 
      scale_y_continuous(
        breaks = c(0, .20, .40, .60, .80, 1),
        labels = c("0%", "20%", "40%", "60%", "80%", "100%"),
        expand = expansion(mult = c(0, 0.1))
      ) +
      theme(
        panel.background = element_rect(fill = "#101010"),
        plot.background = element_rect("#101010", colour = "#E69F00", size = 2),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line("#E69F00"),
        axis.text = element_text(color = "#FFF", size = 20),
        plot.margin = margin(10, 10, 10, 10, "pt"),
        legend.background = element_rect("#101010"),
        legend.text = element_text(color = "#FFF", size = 20),
        plot.title = element_text(
          color = "#FFF", size = 30, hjust = 0.5, 
          margin = margin(15, 0, 15, 0, "pt"))
      ) + 
      labs(title = "Sentiment Prediction")
      
      
    # bg = "#101010",
    # fg = "#FFF",
    # primary = "#E69F00",
    # secondary = "#0072B2",
    # success = "#009E73",
    # "input-border-color" = "#E69F00",
    
    
  })

}