
total = 0
deck.shuffled = shuffler(new("deck"))
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
