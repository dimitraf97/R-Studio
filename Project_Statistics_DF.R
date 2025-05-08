library(naivebayes)   
library(gutenbergr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(e1071) #for naive bayes
library(tm) #for text processing and cleaning 

#initializing mirror and downloading the data
my_mirror <- "http://mirrors.xmission.com/gutenberg/"

df <- gutenberg_metadata

unique(df$author)[startsWith(unique(df$author), "Fl")]
#I ended up using the first two letters of my surname, because while searching with "Flo" the books I got didn't have more than 100.000 words.

gutenberg_works(author == "Flaubert, Gustave")

Madame<-gutenberg_download(2413, mirror = my_mirror)

#tokenizing the text
words_Madame <- unnest_tokens(Madame, word, text)
word_count <- count(words_Madame, word, sort=TRUE)

#calculating relative frequency for words
total_words <- sum(word_count$n)
word_count$relative_frequency <- word_count$n / total_words #relative frequency column was added

#creating bigrams and calculating their relative frequency
bigrams_Madame <- unnest_tokens(Madame, word, text, token = "ngrams", n=2)
bigrams_count <- count(bigrams_Madame, word, sort=TRUE)
bigrams_count$relative_frequency <- bigrams_count$n /total_words #relative frequency in the whole text

#filtering out the NA values from the bigrams list in this way because I have multiple rows with NA values in my list
bigrams_no_NA <- bigrams_count[!is.na(bigrams_count$word), ]

#In my initial attempt to count words and bigrams, I mostly found articles, connectors, and pronouns in the first positions of the word count. 
#For the bigrams, the first entry was 'NA,' which I initially assumed was due to special characters or punctuation. However, I then recalled from your video that most punctuation is excluded during this step. 
#So, I decided not to remove punctuation or stopwords manually, because then the word count would be reduced.

#visualizations with ggplot for word and bigram counts
ggplot_word_count <- ggplot(word_count[1:10,], aes(x = words, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Bar chart +  # Flip coordinates for better readability
  geom_text(aes(label = n), vjust = -0.5) +
  coord_flip() +
  labs(title = "Word Frequency", x = "Words", y = "Frequency") +  # Titles and labels
  theme_minimal() 

ggplot_bigrams_count <- ggplot(bigrams_no_NA[1:10,], aes(x = word, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Bar chart +  # Flip coordinates for better readability
  geom_text(aes(label = n), vjust = -0.5) +
  coord_flip() +
  labs(title = "Word Frequency", x = "Words", y = "Frequency") +  # Titles and labels
  theme_minimal() 

ggplot_bigrams_count

#relative frequency visualization 
barplot(table(word_count$relative_frequency))

#relative frequency visualization of top 5 words
top_5_words <- word_count[order(-word_count$relative_frequency), ][1:5, ]

barplot(top_5_words$relative_frequency, names.arg = top_5_words$word,
        main = "Top 10 Most Frequent Words by Relative Frequency",
        xlab = "Words", ylab = "Relative Frequency", col = "pink", border = "black", ylim = c(0, 0.08))

#relative frequency visualization of top 5 bigrams
top_5_bigrams <- bigrams_no_NA[order(-bigrams_no_NA$relative_frequency), ][1:5, ]


barplot(top_5_bigrams$relative_frequency, names.arg = top_5_bigrams$word,
        main = "Top 10 Most Frequent Words by Relative Frequency",
        xlab = "Words", ylab = "Relative Frequency", col = "purple", border = "black", ylim = c(0, 0.08))

#test for dependence of bigrams <--> phrases
bigrams_no_NA[startsWith(bigrams_no_NA$word,"covered with"),]
bigrams_no_NA[startsWith(bigrams_no_NA$word,"covered "),]
bigrams_no_NA[endsWith(bigrams_no_NA$word,"with"),]

# Create a frequency table for a Chi-squared test
c.w<-bigrams_no_NA[startsWith(bigrams_no_NA$word,"covered with"),]$n
c.notw <- sum(bigrams_no_NA[startsWith(bigrams_no_NA$word,"covered "),]$n)-c.w
notc.w <- sum(bigrams_no_NA[endsWith(bigrams_no_NA$word," with"),]$n)-c.w
notc.notw <- sum(bigrams_no_NA$n) - c.notw - notc.w - c.w  

freq<- matrix(c(c.w,c.notw,notc.w,notc.notw),ncol=2, byrow = T)

mosaicplot(freq) #visualisation in mosaic plot
chisq.test(freq)

#Compute entropy of 1000 word parts
overall_entropy<-c()
for(i in 0:115) #since my word total is 116875
{
  entropy <- words_Madame[(i*1000+1):(i*1000+1000),2]
  char <- unnest_tokens(entropy,token,word, token="characters")#extract the characters
  df.char <- as.data.frame(count(char,token,sort=TRUE))
  df.char$relfreq<-df.char$n/sum(df.char$n) #relative frequency of each token
  df.char$ent<-df.char$relfreq*log2(df.char$relfreq) #we take the relative frequency value and multiply with the logarithm of the frequency
  overall_entropy<- c(overall_entropy, -sum(df.char$ent))
}

overall_entropy #list with all entropies
plot(overall_entropy) #display the plot of all entropies

#Compute the 0.95% confidence interval for entropy
#After calculating the entropies for each token in the text,
#we need to calculate the sample mean and standard deviation

mean_entropy <- mean(overall_entropy)   # Mean of the entropies
sd_entropy <- sd(overall_entropy)       # Standard deviation of the entropies
n <- length(overall_entropy)            # Sample size (number of entropy values)
mean_entropy
sd_entropy
n

# t-critical value
alpha <- 0.05
t_critical <- qt(1 - alpha/2, df = n - 1)

# Margin of error
margin_of_error <- t_critical * (sd_entropy / sqrt(n))

# Confidence interval
ci_lower <- mean_entropy - margin_of_error
ci_upper <- mean_entropy + margin_of_error

# Print the confidence interval
ci_lower
ci_upper
cat("Confidence Interval: [", ci_lower, ", ", ci_upper, "]\n")

#Naive Bayes
#take the overall text and divide it into four datasets
13473/4

Madame1<-Madame[1:3368, ]
Madame2<-Madame[(3368+1):(2*3368),]
Madame3<-Madame[(2*3368+1):(3*3368),]
Madame4<-Madame[(3*3368+1):13473,]

calc_rel_freq <- function(part) {
  word_list <- unlist(strsplit(part$text, split = "\\s+"))
  word_freq <- as.data.frame(table(word_list))
  colnames(word_freq) <- c("word", "n")
  word_freq <- word_freq %>% mutate(relative_frequency = n / sum(n))
  return(word_freq)
}

# Compute the relative frequencies for each part
rel_freqs1 <- calc_rel_freq(Madame1)
rel_freqs2 <- calc_rel_freq(Madame2)
rel_freqs3 <- calc_rel_freq(Madame3)
rel_freqs4 <- calc_rel_freq(Madame4)

naive_bayes <- function(sentence, rel_freqs1, rel_freqs2, rel_freqs3, rel_freqs4) {
  # Convert sentence into words
  words_in_sentence <- unlist(strsplit(sentence, split = " "))
  
  # Compute prior probabilities (uniform in this case, assuming equal likelihood for each part)
  priors <- rep(1 / 4, 4)  # Each part has equal prior probability
  
  # Initialize likelihoods for each part
  likelihood1 <- 1
  likelihood2 <- 1
  likelihood3 <- 1
  likelihood4 <- 1
 
  for (word in words_in_sentence) {
    # Check if word exists in each section and multiply the likelihoods
    likelihood1 <- likelihood1 * ifelse(word %in% rel_freqs1$word, 
                                        rel_freqs1$relative_frequency[rel_freqs1$word == word], 
                                        1e-6)  # Apply smoothing for missing words
    likelihood2 <- likelihood2 * ifelse(word %in% rel_freqs2$word, 
                                        rel_freqs2$relative_frequency[rel_freqs2$word == word], 
                                        1e-6)
    likelihood3 <- likelihood3 * ifelse(word %in% rel_freqs3$word, 
                                        rel_freqs3$relative_frequency[rel_freqs3$word == word], 
                                        1e-6)
    likelihood4 <- likelihood4 * ifelse(word %in% rel_freqs4$word, 
                                        rel_freqs4$relative_frequency[rel_freqs4$word == word], 
                                        1e-6)
  }
    
  # Calculate the posterior probabilities (prior * likelihood)
  posterior1 <- priors[1] * likelihood1
  posterior2 <- priors[2] * likelihood2
  posterior3 <- priors[3] * likelihood3
  posterior4 <- priors[4] * likelihood4
  
  # Store all posterior probabilities in a vector
  posteriors <- c(posterior1, posterior2, posterior3, posterior4)
  
  # Normalize the posteriors so they sum to 1
  posteriors <- posteriors / sum(posteriors)
  
  return(posteriors)
}

# Sentence 1
sentence1 <- "And then I have such a nervous system!"

# Classify sentence 1
posterior_probs1 <- naive_bayes(sentence1, rel_freqs1, rel_freqs2, rel_freqs3, rel_freqs4)

# Output posterior probabilities for sentence 1
posterior_probs1

# Find the part with the highest probability for sentence 1
best_part1 <- which.max(posterior_probs1)
cat("The most likely part for sentence 1 is Madame", best_part1, "\n")

# Sentence 2
sentence2 <- "What are you looking for? asked the master."

# Classify sentence 2
posterior_probs2 <- naive_bayes(sentence2, rel_freqs1, rel_freqs2, rel_freqs3, rel_freqs4)

# Output posterior probabilities for sentence 2
posterior_probs2

# Find the part with the highest probability for sentence 2
best_part2 <- which.max(posterior_probs2)
cat("The most likely part for sentence 2 is Madame", best_part2, "\n")
