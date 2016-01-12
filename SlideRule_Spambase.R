setwd("C:\\Users\\Rebecca\\Documents\\R\\Foundations-of-Data-Science\\Spambase Machine Learning")
sbml <- read.csv("spambase.data")

## explore the data
summary(sbml)
head(sbml)
str(sbml)

## rename the variables
sbml2 <- sbml
colnames(sbml2) <- c("word_freq_make", "word_freq_address", "word_freq_all", "word_freq_3d",
                     "word_freq_our", "word_freq_over", "word_freq_remove", "word_freq_internet",
                     "word_freq_order", "word_freq_mail", "word_freq_receive", "word_freq_will",
                     "word_freq_people", "word_freq_report", "word_freq_addresses", "word_freq_free",
                     "word_freq_business", "word_freq_email", "word_freq_you", "word_freq_credit",
                     "word_freq_your", "word_freq_font", "word_freq_000", "word_freq_money",
                     "word_freq_hp", "word_freq_hpl", "word_freq_george", "word_freq_650",
                     "word_freq_lab", "word_freq_labs", "word_freq_telnet", "word_freq_857",
                     "word_freq_data", "word_freq_415", "word_freq_85", "word_freq_technology",
                     "word_freq_1999", "word_freq_parts", "word_freq_pm", "word_freq_direct",
                     "word_freq_cs", "word_freq_meeting", "word_freq_original", "word_freq_project",
                     "word_freq_re", "word_freq_edu", "word_freq_table", "word_freq_conference",
                     "char_freq_semi", "char_freq_startp", "char_freq_startb", 
                     "char_freq_excl", "char_freq_dollar", "char_freq_pound",
                     "capital_run_length_average",
                     "capital_run_length_longest",
                     "capital_run_length_total",
                     "is_spam")
str(sbml2)
summary(sbml2)

## decision tree to see dominant predictors of is_spam
library(tree)
tree.sbml2 <- tree(is_spam ~., data = sbml2)
summary(tree.sbml2) # gives the variables entered in the tree model
plot(tree.sbml2)
text(tree.sbml2,pretty=0)

## overlay some variables with is_spam
par(mfrow=c(2,2))
plot(sbml2$is_spam~sbml2$char_freq_dollar)
plot(sbml2$is_spam~sbml2$capital_run_length_total)
plot(sbml2$is_spam~sbml2$word_freq_edu)
plot(sbml2$is_spam~sbml2$word_freq_george)

## some histograms
library(gridExtra)
h1 <- qplot(x = char_freq_dollar, data = sbml2,
            binwidth = 0.5)
h2 <- qplot(x = log(capital_run_length_total), data = sbml2)
h3 <- qplot(x = word_freq_our, data = sbml2,
            binwidth = 0.5)
h4 <- qplot(x = char_freq_pound, data = sbml2,
            binwidth = 0.5)
grid.arrange(h1, h2, h3, h4, ncol = 2)