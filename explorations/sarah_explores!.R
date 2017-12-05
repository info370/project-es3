library(ggplot2)
library(base)
library(car)

data <- read.csv('../data/clean_num.csv')
no_out <- filter(data, online_job_postings < 500)
short_search <- filter(data, Months <= 3) 

postings.lm <- lm(online_job_postings ~ networking, no_out)
summary(postings.lm)
plot(resid(postings.lm))
?lm

ggplot(no_out, aes(x = networking, y = online_job_postings)) +
  geom_point(alpha = 0.1 ) + 
  geom_smooth(method=loess) 

ggplot(no_out, aes(x = gender, y = self.confidence)) +
  geom_point(alpha = 0.1 ) + 
  geom_smooth(method=loess) 

#cook's distance > 4/n can remove
plot(cooks.distance(lm(online_job_postings ~ good_at_resume_cover_letters, data)))

ggplot(data, aes(x = good_at_resume_cover_letters, y = cover_letter_hours)) +
  geom_point(alpha = 0.1 ) + 
  geom_smooth(method=lm) 

ggplot(no_out, aes(x = online_job_postings, y = resume_hrs)) +
  geom_point(alpha = 0.1 ) + 
  geom_smooth(method=loess) 

ggplot(no_out, aes(x = online_job_postings, y = cover_letter_hours)) +
  geom_point(alpha = 0.1 ) + 
  geom_smooth(method=loess) 

no_out <- mutate(no_out, total_time)