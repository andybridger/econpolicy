install.packages("devtools")

library(devtools)
install_github("andybridger/aigtheme")

library(aigtheme)

# library
library(ggplot2)
library (readr)

#download csv from Github
urlfile="https://raw.githubusercontent.com/lrjoshi/webpage/master/public/post/c159s.csv"
data<-read_csv(url(urlfile))

# Keep 30 first rows in the mtcars natively available dataset
data=head(mtcars, 30)

# Add one annotation
ggplot(data, aes(x=wt, y=mpg)) +
  geom_point() + # Show dots
  geom_label(
    label="Look at this!", 
    x=4.1,
    y=20,
    label.padding = unit(0.55, "lines"), # Rectangle size around label
    label.size = 0.35,
    color = "black",
    fill="#69b3a2"
  )+
  andy_style()+
  labs(title = "Job vacancies in Regional Australia are at a record high",
       subtitle = "Regional online vacancies, 000s per month",
       caption = "Data are a three-month-average from the Internet Vacancies Index. These data are based on a count of new online job advertisements lodged\non Seek, CareerOne and Australian JobSearch during the month.")


library(ggplot2)
#'
ggplot(data = mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
geom_point() +
andy_colour_manual(n = 3) +
andy_style()


p <- ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
geom_point() +
andy_style() +
scale_colour_manual(values = andy_pal(n = 3))
p
