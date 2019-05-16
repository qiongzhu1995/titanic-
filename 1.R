titanic$title[titanic$title == ' Mlle']        <- 'Miss' 

titanic$title[titanic$title == ' Ms']          <- 'Miss'

titanic$title[titanic$title == ' Mme']         <- 'Mrs' 

titanic$title[titanic$title == ' Lady']          <- 'Miss'

titanic$title[titanic$title == ' Dona']          <- 'Miss'
titanic$title[titanic$title == 'Capt']        <- 'Officer' 

titanic$title[titanic$title == 'Col']        <- 'Officer' 

titanic$title[titanic$title == 'Major']   <- 'Officer'

titanic$title[titanic$title == 'Dr']   <- 'Officer'

titanic$title[titanic$title == 'Rev']   <- 'Officer'

titanic$title[titanic$title == 'Don']   <- 'Officer'

titanic$title[titanic$title == 'Sir']   <- 'Officer'

titanic$title[titanic$title == 'the Countess']   <- 'Officer'

titanic$title[titanic$title == 'Jonkheer']   <- 'Officer'  
ticket.unique <- rep(0, nrow(full))

tickets <- unique(titanic$Ticket)



for (i in 1:length(tickets)) {
  
  current.ticket <- tickets[i]
  
  party.indexes <- which(titanic$Ticket == current.ticket)
  
  
  for (k in 1:length(party.indexes)) {
    
    ticket.unique[party.indexes[k]] <- length(party.indexes)
    
  }
  
}



titanic$ticket.unique <- ticket.unique

titanic$ticket.size[titanic$ticket.unique == 1]   <- 'Single'

titanic$ticket.size[titanic$ticket.unique < 5 &titanic$ticket.unique>= 2]   <- 'Small'

titanic$ticket.size[titanic$ticket.unique >= 5]   <- 'Big'
