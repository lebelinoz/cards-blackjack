deck <- setClass("deck",
                 slots = c(
                   the.cards = "data.frame",
                   pointer = "numeric",
                   shuffled = "logical"
                 ),
                 prototype = list(
                   the.cards = data.frame(name = rep(c("A", 2:10, "J", "Q", "K"), 4), value = rep(c(1:10, 10, 10, 10), 4), suit = rep(c("Heart", "Diamond", "Club", "Spade"), each = 13)),
                   pointer = 1,
                   shuffled = FALSE
                 ),
                 validity = function(obj) {
                   if (obj@pointer > 0 && obj@pointer <= nrow(obj@the.cards)) {
                     return(TRUE)
                   } else {
                     return("pointer must be an index of the.cards")
                   }
                 }
)

# Shuffle the deck:
setGeneric(name = "shuffle", def = function(obj) { standardGeneric("shuffle") })
setMethod(f = "shuffle", signature = "deck", definition = function(obj) {
    obj@the.cards = obj@the.cards[sample(row.names(obj@the.cards)),]
    obj@shuffled = TRUE
    return(obj) 
  })

# Take a card:
setGeneric(name = "take.a.card", def = function(obj) { standardGeneric("take.a.card") })
setMethod(f = "take.a.card", signature = "deck", definition = function(obj) {
    if (obj@pointer <= nrow(obj@the.cards)) {
      card - obj@the.cards[obj@pointer,]
    } else {
      return(NULL)
    }
})
