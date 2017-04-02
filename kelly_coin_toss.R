#####################################################################
## Kelly Criterion applied to a biased coin toss game
##
##  Assume you can bet infinitely large and infinitessimaly small amounts on a biased coin toss, as many times as you wish.
##  When the coin returns heads, win your bet (+1).  When tails, lose your bet (-1).
##  Start with an initial pool of $1, and try different betting techniques:  fixed, all-or-nothing, martingale or Kelly.
##
##  Inspired by William Poundstone explanation of the Kelly Criteria in "Fortune's Formula: The Untold Story of (...)"
#####################################################################
library(ggplot2)  
library(dplyr)

number_of_trials = 500
probability_of_heads = 0.55
coin_toss = sample(c(-1, 1), number_of_trials, replace = TRUE, prob = c(1 - probability_of_heads, probability_of_heads))

## If martingale looks better then Kelly (which sometimes happen, let the experiment run another 500 trials by doing the following:
#coin_toss = c(coin_toss, sample(c(-1, 1), number_of_trials, replace = TRUE, prob = c(1 - probability_of_heads, probability_of_heads)))
#number_of_trials = 2 * number_of_trials

# 1. Fixed Wager.  Bet 10 cents each toss:
payoff1 = cumsum(coin_toss * 0.1) + rep(1, number_of_trials)
df = data.frame(system = "Fixed Wager", payoff = payoff1, trial = 1:number_of_trials)


# 2. Bet-it-all.  Bet double or nothing with entire pool for as long as you can.
payoff2 = rep(0, times = number_of_trials)
i = 1
trial = coin_toss[i]
while(trial > 0) {
    payoff2[i] = trial
    i = i + 1
    trial = trial + trial * coin_toss[i]
}
df = rbind(df, data.frame(system = "Bet-it-all", payoff = payoff2, trial = 1:number_of_trials))


# 3. Martingale.  When you lose, bet double-or-nothing until you recoup your losses
martingale = rep(0, times = number_of_trials)
initial_bet_size = 0.1
bet_size = initial_bet_size
betting_pool = 1
for (i in 1:number_of_trials) {
    betting_pool = betting_pool + bet_size * coin_toss[i]
    martingale[i] = betting_pool
    if (coin_toss[i] < 0) {
        bet_size = 2 * bet_size
    } else {
        bet_size = initial_bet_size
    }
}
payoff3 = martingale
df = rbind(df, data.frame(system = "Martingale", payoff = payoff3, trial = 1:number_of_trials))


## 4. Margingale with table limit.
##      When you lose, bet double-or-nothing until you recoup your losses.  But there's a maximum bet.
#table_limit = 20
#martingale = rep(0, times = number_of_trials)
#bet_size = 1
#for (i in 1:number_of_trials) {
    #martingale[i] = bet_size * coin_toss[i]
    #if (coin_toss[i] < 0) {
        #bet_size = 2 * bet_size
    #} else {
        #bet_size = 1

    #}
    #bet_size = min(bet_size, table_limit)
#}
#payoff4 = cumsum(martingale * 0.1) + rep(1, number_of_trials)
#df = rbind(df, data.frame(system = "Martingale with table limit", payoff = payoff4, trial = 1:number_of_trials))

# 5. Kelly.  Bet a % of your pool equal to your edge.
proportion = probability_of_heads - (1 - probability_of_heads)
payoff5 = rep(0, times = number_of_trials)
betting_pool = 1
for (i in 1:number_of_trials) {
    if (i > 1) betting_pool = payoff5[i - 1]
    betting_pool = betting_pool + proportion * betting_pool * coin_toss[i]
    payoff5[i] = betting_pool
}
df = rbind(df, data.frame(system = "Kelly", payoff = payoff5, trial = 1:number_of_trials))

# Plot!
#ggplot(df, aes(trial, payoff, color = system)) + geom_line() + theme(legend.position = "bottom")


# Plot the knock-out version:
df_no_ko = mutate(df, knockout = "Can fall below zero")
df_ko = mutate(df, knockout = "Stop after going bust")

# For each system, find the first instance when the betting pool falls below zero.  Make everything after that point zero.
systems = unique(df$system)
for (s in systems) {
    bust_indices = which(df_ko$system == s & df_ko$payoff <= 0)
    if (length(bust_indices) > 0) {
        bust_index = min(bust_indices)
        system_index = which(df_ko$system == s)
        busted_system_index = system_index[which(system_index >= bust_index)]
        df_ko[busted_system_index, "payoff"] = 0
    }
}

dfko = rbind(df_no_ko, df_ko)
ggplot(dfko, aes(trial, payoff, color = system)) + geom_line() + theme(legend.position = "bottom") + facet_wrap(~knockout, ncol = 1, scales = "free")

