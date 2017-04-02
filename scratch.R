card.names = c("A", 2:10, "J", "Q", "K")
deck = data.frame(name = rep(card.names, 4), value = rep(c(1:10, 10, 10, 10), 4), suit = rep(c("Heart", "Diamond", "Club", "Spade"), each = 13))

pop = function(df) {
  l = list(top = df[1,], rest = df[-1,])
}

total = 0
deck.shuffled = deck[sample(row.names(deck)),]
#hand = data.frame(name = character(0), value = numeric(0), suit = character(0))
while (total <= 17)
{
  l = pop(deck.shuffled)
  card = l$top
  deck.shuffled = l$rest
  total = total + card[1,]$value
  cat(as.character(card[1,]$name), " of ", as.character(card[1,]$suit), "s - total = ", total, "\n")
  if (total == 21) cat("Blackjack!")
  if (total > 21) cat("Bust!")
  #hand = rbind(hand, card[1,])
}
