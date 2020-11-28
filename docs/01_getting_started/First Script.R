roll3 <- function(faces = 1:6, number_of_dice = 1) {
  dice <- sample(x = faces, size = number_of_dice, 
                 replace = TRUE, 
                 prob = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5))
  sum(dice)
}

results <- replicate(n = 100, expr = roll3(), simplify=TRUE)
hist(results)