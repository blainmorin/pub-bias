require(dplyr) || install.packages("dplyr")
require(meta) || install.packages("meta")
require(metasens) || install.packages("metasens")
require(readr) || install.packages("readr")


### Put file path here
X2010_6 <- read_delim("2010_6.csv", "\t", 
                      escape_double = FALSE, trim_ws = TRUE)


### Create subset
### Lin et al use meta analysis with at least five studies
### Lin et al use odds ratios for binary outcomes

small.data = X2010_6 %>% 
          filter(number_review >= 1, 
                 number_review <= 500,
                 no_of_studies >= 5)



### Add number of obs column 
small.data = small.data %>% mutate(obs = a + b + c +d)



### Function to retrieve odds ratio

get.or = function (a, b, c, d){
  
  the.meta = metabin(event.e = a, n.e = a + c, event.c = b, n.c = b + d, sm = "OR")
  exp(the.meta$TE.fixed)
  
}


### Function to retrieve CI lower bound

get.lb = function (a, b, c, d){
  
  the.meta = metabin(event.e = a, n.e = a + c, event.c = b, n.c = b + d, sm = "OR")
  exp(the.meta$lower.fixed)
  
}


### Function to retrieve CI upper bound

get.ub = function (a, b, c, d){
  
  the.meta = metabin(event.e = a, n.e = a + c, event.c = b, n.c = b + d, sm = "OR")
  exp(the.meta$upper.fixed)
  
}



### Get odds ratios and CI for each outcome

truth = small.data %>% group_by(my_ID) %>%
  mutate(true.OR = get.or(a = a, b = b, c = c, d = d),
         lowerCI = get.lb(a = a, b = b, c = c, d = d),
         upperCI = get.ub(a = a, b = b, c = c, d = d))


### Function that drops small studies (by sample size)

drop.proportion = function(df, proportion) {
  
  new.df = df %>%
    group_by(.data$my_ID) %>%
    arrange(desc(.data$obs), .by_group = TRUE) %>%
    slice( seq((1-proportion) * n()) )
  
  return(new.df)
  
}



### Example: Remove the smallest 20% (by sample size)

biased.data = drop.proportion(small.data, .2)

### Recalculate OR and CI

half.truth = biased.data %>% group_by(my_ID) %>%
  mutate(OR = get.or(a = a, b = b, c = c, d = d),
         lowerCI = get.lb(a = a, b = b, c = c, d = d),
         upperCI = get.ub(a = a, b = b, c = c, d = d))


test = half.truth %>% filter(my_ID == 33)

test.meta = metabin(event.e = a, n.e = a + c, event.c = b, n.c = b + d, data = test, sm = "OR")

test.copas = copas(test.meta)

test.trimfill = trimfill(test.meta)
test.trimfill$
trimfill(test.meta)$TE.fixed
summary(test.trimfill)

added.trim = half.truth %>%
  group_by(my_ID) %>% 
  mutate(new_n_of_studies = n()) %>%
  filter(new_n_of_studies >= 5) %>%
  mutate(trimfill.OR = exp(trimfill( metabin(event.e = a, n.e = a + c, event.c = b, n.c = b + d, sm = "OR"))$TE.random))

interm = truth %>%
  select(-lowerCI, -upperCI)


joinedwithtruth = inner_join(x = added.trim, y = interm) %>%
  group_by(my_ID) %>%
  slice(1)

