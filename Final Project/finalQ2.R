source("https://www.math.hmc.edu/m35f/FinalLoader.R")
sqf2010 = read.csv("2010_sqf_m35.csv")
## sqf2015 = read.csv("2015_sqf_m35.csv")

#######QUESTIOIN 1###########
table(sqf2010$race)

# we have two unknowns and two eauqtions:
# .95(total hispanics) + .05(total API) = total num identified as hispanic
# .95(total API) + .05(total hispanics) = total num identified as API
# we solve this system to see the results below

totH <- 198748
totAPI <- 10310.1

# after doing these calcs, we see rateH = .262 and rateAPI = 0.021
tot <- 601285
rateH <- totH/tot
rateAPI <- totAPI/tot

# now we use bayes law
rateCorrIdH <- .95
rateIdH <- (150637+38689)/tot
rateHgivenIdH <- rateCorrIdH*rateH/rateIdH

# round 2
rateCorrIdAPI <- .95
rateIdAPI <- 19732/tot
rateAPIgivenIdAPI <- rateCorrIdAPI*rateAPI/rateIdAPI

# we found that the probability someone is actually hispanic given that they
# were identified as a hispanic is 99.73%


# since the hispanic group is much larger, the 5% of misidentified API people barely
# makes a dent in the group identified as Hispanic. Thus, it is highly probable that 
# someone identified as hispanic is actually hispanic.

# Conversely, by looking at the table of races, we see that the true number of API people
# is 9000 less than the number identified as API - almost 50%! This is becuase the hispanic 
# group is just so much bigger, so the 5% of hispanics misidentified as API makes up a large 
# portion of the group identified as API.


# The value observed by just a quick look at the table is confirmed by analysis
# applying Bayes' rule. We found that of the probability that someone is actually API
# given that they are identified as API is 49.6% - that's tiny!!! That means over half 
# the people identified as API are actually hispanic - a false positive rate of 50.4%.

# After these analyses, we learn that we must take the statistics reported in the race table
# (and honetly most other stats - mistakes can happen) must be taken with a grain of salt, as 
# they may have high error rates.

#######QUESTION 2 #########

# let's take a quick first look at the demographic breakdown of frisk statistics 
table(sqf2010$frisked, sqf2010$race)

# this table only presents totals of occurrences of being frisked and occurrences
# of not being frisked by race, which is not entirely helpful on it's own to make
# conclusions -- we need more analysis.

# What we can do is compare sample distributions of 1's and 0's and run a two sample
# t-test that compares the true distributions. We do this because the mean of each 
# distribution actually represents the rate at which each group is frisked. If we show 
# one group's sample mean is significantly greater than another group's sample mean,
# we can state with confidence that some demographic groups are more likely to be frisked than others.

# here we collect distributions for each race. Each of the following vectors contains a list
# of 1s and 0s. Type "friskedBL" and hit enter after running the following code for an example.
friskedBL <-sqf2010$frisked[sqf2010$race=="BLACK"] 
friskedWH <-sqf2010$frisked[sqf2010$race=="WHITE"]
friskedBLH <-sqf2010$frisked[sqf2010$race=="BLACK-HISPANIC"]
friskedH <-sqf2010$frisked[sqf2010$race=="WHITE-HISPANIC"]
friskedAPI <-sqf2010$frisked[sqf2010$race=="ASIAN/PACIFIC ISLANDER"]
friskedNative <-sqf2010$frisked[sqf2010$race=="AMERICAN INDIAN/ALASKAN NATIVE"]

# Now we run our t-tests. While we may have some premonitions that certain groups are profiled over others
# we will still run a two-sided test because any significant difference in frisk rates - even a surprising one
# - will be in contrary to the null hypothesis claimed by SQF supporters that racial profiling is not practiced
# or implemented in SQF. 

# There are 15 (6 choose 2) tests to run.
t.test(friskedH,friskedWH,alternative="two.sided")
t.test(friskedBL,friskedWH,alternative="two.sided")
t.test(friskedBLH,friskedWH,alternative="two.sided")
t.test(friskedAPI,friskedWH,alternative="two.sided")
t.test(friskedNative,friskedWH,alternative="two.sided")
t.test(friskedBL,friskedH,alternative="two.sided")
t.test(friskedBL,friskedBLH,alternative="two.sided")
t.test(friskedBL,friskedAPI,alternative="two.sided")
t.test(friskedBL,friskedNative,alternative="two.sided")
t.test(friskedH,friskedBLH,alternative="two.sided")
t.test(friskedH,friskedAPI,alternative="two.sided")
t.test(friskedH,friskedNative,alternative="two.sided")
t.test(friskedBLH,friskedAPI,alternative="two.sided")
t.test(friskedBLH,friskedNative,alternative="two.sided")
t.test(friskedNative,friskedAPI,alternative="two.sided")

# See our final paper for a summarized version of the results.

#The same method above will be applied to frisk rates broken down by borrough.

#Below are data for frisk rates broken down by borrough. There are 
#significant differences in rates between every borrough except BKN and MAN.
#Order (from highest rate to lowest): BRX, QNS, BKN, MAN, STI
#Biggest diff: BRX and STI
#Smallest diff: BKN and MAN
table(sqf2010$frisked,sqf2010$city)
friskedBRONX <- sqf2010$frisked[sqf2010$city=="BRONX"]
friskedBKN <- sqf2010$frisked[sqf2010$city=="BROOKLYN"]
friskedQNS <- sqf2010$frisked[sqf2010$city=="QUEENS"]
friskedSTI <- sqf2010$frisked[sqf2010$city=="STATEN IS"]
friskedMAN <- sqf2010$frisked[sqf2010$city=="MANHATTAN"]
t.test(friskedBRONX,friskedMAN)
t.test(friskedBRONX,friskedBKN)
t.test(friskedBRONX,friskedQNS)
t.test(friskedBRONX,friskedSTI) #Biggest diff: BRX and STI
t.test(friskedBKN,friskedMAN)
t.test(friskedBKN,friskedQNS)
t.test(friskedBKN,friskedSTI)
t.test(friskedQNS,friskedMAN)
t.test(friskedQNS,friskedSTI)
t.test(friskedMAN,friskedSTI) #Smallest diff: BKN and MAN

# let's check to see if there could be any correlations between demographics of a 
# borrough and the rate at which its citizens are frisked. Let's check the two extremes:
# Bronx and Staten Island

table(sqf2010$race[sqf2010$city=="BRONX"])
table(sqf2010$race[sqf2010$city=="STATEN IS"])

# Both borroughs have a plurality of black residents, but when we look at the sum of black and 
# hispanic residents, we find the following proportions

minoritiesBRX <- 53987 + 12448 + 35778
minoritiesSTI <- 11401 + 939 + 4953

#table(sqf2010$city) # let's find out total populations
totBRX <- 112415
totSTI <- 27501

propMinBRX <- minoritiesBRX/totBRX
propMinSTI <- minoritiesSTI/totSTI
propMinBRX <- 53987/totBRX # this turns out to be 48.0%
propMinSTI <- 11401/totSTI # this turns out to be 41.5%

# In lieu of doing another test, I will refer back to prior tests we ran in which differences in sample means
# as small as 1% were significant. Here we have a difference of 6.5%. From our previous tests, we can comfortably
# say that the there the minority group in BRX is much larger than the minority group in STI. 
