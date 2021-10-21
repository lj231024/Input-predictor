#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

####################################


shinyServer(function(input, output, session) {
    
    isolate({
        withProgress(message = 'Loading...',
                     detail = 'This may take several minutes...', value = 0, {
                         library(tidytext)
                         library(tidyr)
                         library(dplyr)
                         library(ggplot2)
                         library(qdapRegex)
                         library(ngram)
                         library(RWeka)
                         library(tm)
                         library(quanteda)
                         library(stringi)
                         #quan_txt <- readRDS("./quan_txt.rds")
                         quan_txt_fast <- readRDS("./quan_txt_fast.rds")
                         
                         

                     })
    })
    
   
    
    
    
    
  
    
    output$predict_input <-  renderText({
      
      model <- input$rb
      
      if(model == "Fast mode"){
      
        withProgress(message = 'Processing... predict with fast mode',
                     detail = 'please do not press the predict button again...', value = 0, {
                         
                         input <- input$Inputtext
                         split <- strsplit(input, split =' ')
                         split_unlist <- unlist(split)
                         split_lastthree <- split_unlist[(length(split_unlist)-2):length(split_unlist)]
                         quan_grep <-quan_txt_fast[grep(paste("\\b", split_lastthree[2],"\\b",".*",
                                                         "\\b", split_lastthree[3],"\\b", sep = ''),quan_txt_fast)]
                         
                         if(length(quan_grep) == 0){return("the")} 
                         
                         quan_grep_df <- data_frame(line = 1:length(quan_grep), text = quan_grep)
                         
                         #################
                         
                         quan_grep_two <- quan_grep_df %>%
                             unnest_tokens(text, text, token = "ngrams", n = 2) 
                         
                         quan_grep_two_txt <- quan_grep_two$text
                         
                         
                         two_grep <-quan_grep_two_txt [grep(paste("^", split_lastthree[3], sep = ''),quan_grep_two_txt )]
                         
                         quan_grep_df <- data_frame(line = 1:length(two_grep), text = two_grep)
                         
                         
                         TworamFreq <- quan_grep_df %>%
                             unnest_tokens(sixgram, text, token = "ngrams", n = 2) %>%
                             separate(sixgram, c("word1", "word2"), sep = " ", 
                                      extra = "drop", fill = "right") %>%
                             filter(!word1 == "NA",
                                    !word2 == "NA") %>%
                             unite(sixgram, word1, word2, sep = " ") %>%
                             count(sixgram, sort = TRUE)
                         
                         
                         predict_input1 <- unlist(strsplit(as.character(TworamFreq[1,1]), split = ' '))[2]
                         predict_input2 <-unlist(strsplit(as.character(TworamFreq[2,1]), split = ' '))[2]
                         predict_input3 <-unlist(strsplit(as.character(TworamFreq[3,1]), split = ' '))[2]
                         predict_input4 <-unlist(strsplit(as.character(TworamFreq[4,1]), split = ' '))[2]
                         predict_input5 <-unlist(strsplit(as.character(TworamFreq[5,1]), split = ' '))[2]
                         
                         predict_input <- as.data.frame(rbind(predict_input1,predict_input2,predict_input3,predict_input4,predict_input5))
                         predict_input <- cbind(predict_input, TworamFreq[1:5,2])
                         
                         colnames(predict_input) <- c("Word", "Freq")
                         
                        
                         
                         predict_input[1,1]})
        
      }
      
      else if(model == "Complete mode"){
        
#        withProgress(message = 'Processing... predict with Complete mode',
#                     detail = 'please do not press the predict button again...', value = 0, {
#                       
#                       input <- input$Inputtext
#                       split <- strsplit(input, split =' ')
#                       split_unlist <- unlist(split)
#                       split_lastthree <- split_unlist[(length(split_unlist)-2):length(split_unlist)]
#                       quan_grep <-quan_txt[grep(paste("\\b", split_lastthree[2],"\\b",".*",
#                                                            "\\b", split_lastthree[3],"\\b", sep = ''),quan_txt)]
#                       
#                       if(length(quan_grep) == 0){return("the")} 
#                       
#                       quan_grep_df <- data_frame(line = 1:length(quan_grep), text = quan_grep)
#                       
#                       #################
#                       
#                       quan_grep_two <- quan_grep_df %>%
#                         unnest_tokens(text, text, token = "ngrams", n = 2) 
#                       
#                       quan_grep_two_txt <- quan_grep_two$text
#                       
#                       
#                       two_grep <-quan_grep_two_txt [grep(paste("^", split_lastthree[3], sep = ''),quan_grep_two_txt )]
#                       
#                       quan_grep_df <- data_frame(line = 1:length(two_grep), text = two_grep)
#                       
#                       
#                       TworamFreq <- quan_grep_df %>%
#                         unnest_tokens(sixgram, text, token = "ngrams", n = 2) %>%
#                         separate(sixgram, c("word1", "word2"), sep = " ", 
#                                  extra = "drop", fill = "right") %>%
#                         filter(!word1 == "NA",
#                                !word2 == "NA") %>%
#                         unite(sixgram, word1, word2, sep = " ") %>%
#                         count(sixgram, sort = TRUE)
#                       
#                       
#                       predict_input1 <- unlist(strsplit(as.character(TworamFreq[1,1]), split = ' '))[2]
#                       predict_input2 <-unlist(strsplit(as.character(TworamFreq[2,1]), split = ' '))[2]
#                       predict_input3 <-unlist(strsplit(as.character(TworamFreq[3,1]), split = ' '))[2]
#                       predict_input4 <-unlist(strsplit(as.character(TworamFreq[4,1]), split = ' '))[2]
#                       predict_input5 <-unlist(strsplit(as.character(TworamFreq[5,1]), split = ' '))[2]
#                       
#                       predict_input <- as.data.frame(rbind(predict_input1,predict_input2,predict_input3,predict_input4,predict_input5))
#                       predict_input <- cbind(predict_input, TworamFreq[1:5,2])
#                       
#                       colnames(predict_input) <- c("Word", "Freq")
#                       
#                       
#                       predict_input[1,1]})
        
        "Sorry, shinny app can not load the big dataset. Please run the code on your local server."
        
      }
        
    })
    
    output$txt <-  renderText({
      
      model <- input$rb
      
      if(model == "Fast mode"){
        
        withProgress(message = 'Processing... predict with fast mode',
                     detail = 'please do not press the predict button again...', value = 0, {
      
      
      
      input <- input$Inputtext
      split <- strsplit(input, split =' ')
      split_unlist <- unlist(split)
      split_lastthree <- split_unlist[(length(split_unlist)-2):length(split_unlist)]
      quan_grep <-quan_txt_fast[grep(paste("\\b", split_lastthree[2],"\\b",".*",
                                           "\\b", split_lastthree[3],"\\b", sep = ''),quan_txt_fast)]
      
      if(length(quan_grep) == 0){return(paste(input, "the", sep = ' '))} 
      
      quan_grep_df <- data_frame(line = 1:length(quan_grep), text = quan_grep)
      
      
      
      #################
      
      quan_grep_two <- quan_grep_df %>%
        unnest_tokens(text, text, token = "ngrams", n = 2) 
      
      quan_grep_two_txt <- quan_grep_two$text
      
      
      two_grep <-quan_grep_two_txt [grep(paste("^", split_lastthree[3], sep = ''),quan_grep_two_txt )]
      
      quan_grep_df <- data_frame(line = 1:length(two_grep), text = two_grep)
      
      
      TworamFreq <- quan_grep_df %>%
        unnest_tokens(sixgram, text, token = "ngrams", n = 2) %>%
        separate(sixgram, c("word1", "word2"), sep = " ", 
                 extra = "drop", fill = "right") %>%
        filter(!word1 == "NA",
               !word2 == "NA") %>%
        unite(sixgram, word1, word2, sep = " ") %>%
        count(sixgram, sort = TRUE)
      
      
      predict_input1 <- unlist(strsplit(as.character(TworamFreq[1,1]), split = ' '))[2]
      predict_input2 <-unlist(strsplit(as.character(TworamFreq[2,1]), split = ' '))[2]
      predict_input3 <-unlist(strsplit(as.character(TworamFreq[3,1]), split = ' '))[2]
      predict_input4 <-unlist(strsplit(as.character(TworamFreq[4,1]), split = ' '))[2]
      predict_input5 <-unlist(strsplit(as.character(TworamFreq[5,1]), split = ' '))[2]
      
      predict_input <- as.data.frame(rbind(predict_input1,predict_input2,predict_input3,predict_input4,predict_input5))
      predict_input <- cbind(predict_input, TworamFreq[1:5,2])
      
      colnames(predict_input) <- c("Word", "Freq")
      
      
      paste(input, predict_input[1,1], sep = ' ')}
      
        )}
      
      
      
      
      else if(model == "Complete mode"){
        
#        withProgress(message = 'Processing... predict with complete mode',
#                     detail = 'please do not press the predict button again...', value = 0, {
#        
#        input <- input$Inputtext
#        split <- strsplit(input, split =' ')
#        split_unlist <- unlist(split)
#        split_lastthree <- split_unlist[(length(split_unlist)-2):length(split_unlist)]
#        quan_grep <-quan_txt[grep(paste("\\b", split_lastthree[2],"\\b",".*",
#                                             "\\b", split_lastthree[3],"\\b", sep = ''),quan_txt)]
#        
#        if(length(quan_grep) == 0){return(paste(input, "the", sep = ' '))} 
#        
#        quan_grep_df <- data_frame(line = 1:length(quan_grep), text = quan_grep)
#        
#        
#        
#        #################
#        
#        quan_grep_two <- quan_grep_df %>%
#          unnest_tokens(text, text, token = "ngrams", n = 2) 
#        
#        quan_grep_two_txt <- quan_grep_two$text
#        
#        
#        two_grep <-quan_grep_two_txt [grep(paste("^", split_lastthree[3], sep = ''),quan_grep_two_txt )]
#        
#        quan_grep_df <- data_frame(line = 1:length(two_grep), text = two_grep)
#        
#        
#        TworamFreq <- quan_grep_df %>%
#          unnest_tokens(sixgram, text, token = "ngrams", n = 2) %>%
#          separate(sixgram, c("word1", "word2"), sep = " ", 
#                   extra = "drop", fill = "right") %>%
#          filter(!word1 == "NA",
#                 !word2 == "NA") %>%
#          unite(sixgram, word1, word2, sep = " ") %>%
#          count(sixgram, sort = TRUE)
#        
#        
#        predict_input1 <- unlist(strsplit(as.character(TworamFreq[1,1]), split = ' '))[2]
#        predict_input2 <-unlist(strsplit(as.character(TworamFreq[2,1]), split = ' '))[2]
#        predict_input3 <-unlist(strsplit(as.character(TworamFreq[3,1]), split = ' '))[2]
#        predict_input4 <-unlist(strsplit(as.character(TworamFreq[4,1]), split = ' '))[2]
#        predict_input5 <-unlist(strsplit(as.character(TworamFreq[5,1]), split = ' '))[2]
#        
#        predict_input <- as.data.frame(rbind(predict_input1,predict_input2,predict_input3,predict_input4,predict_input5))
#        predict_input <- cbind(predict_input, TworamFreq[1:5,2])
#        
#        colnames(predict_input) <- c("Word", "Freq")
#        
#        
#        paste(input, predict_input[1,1], sep = ' ')})
        
        "Sorry, shinny app can not load the big dataset. Please run the code on your local server."
      
      }

})

})
