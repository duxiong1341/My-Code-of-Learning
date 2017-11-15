
library(readr)
diamonds <- read_csv("C:\\Users\\Think\\Desktop\\ggplot2-master\\data-raw\\diamonds.csv")

set.seed(1410) # Make the sample reproducible
dsmall <- diamonds[sample(nrow(diamonds), 100), ]

library(ggplot2)
qplot(carat, price, data = diamonds)

qplot(log(carat), log(price), data = diamonds)

qplot(carat, x * y * z, data = diamonds)

# Mapping point colour to diamond colour (left), and point shape to cut
# quality (right).
# color 和 cut 在这里是变量
qplot(carat, price, data = dsmall, colour = color)
qplot(carat, price, data = dsmall, shape = cut)

# Reducing the alpha value from 1/10 (left) to 1/100 (middle) to 1/200
# (right) makes it possible to see where the bulk of the points lie.
# 
qplot(carat, price, data = diamonds, alpha = I(1/10))
qplot(carat, price, data = diamonds, alpha = I(1/100))
qplot(carat, price, data = diamonds, alpha = I(1/200))

# Smooth curves add to scatterplots of carat vs.\ price. The dsmall
# dataset (left) and the full dataset (right).
qplot(carat, price, data = dsmall, geom = c("point", "smooth"))
qplot(carat, price, data = diamonds, geom = c("point", "smooth"))

# The effect of the span parameter.  (Left) \code{span = 0.2}, and
# (right) \code{span = 1}.
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), 
  span = 0.2)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), 
  span = 1)

# The effect of the formula parameter, using a generalised additive
# model as a smoother.  (Left) \code{formula = y ~ s(x)}, the default;
# (right) \code{formula = y ~ s(x, bs = "cs")}.
library(mgcv)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), 
  method = "gam", formula = y ~ s(x))
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), 
  method = "gam", formula = y ~ s(x, bs = "cs"))

# The effect of the formula parameter, using a linear model as a
# smoother.  (Left) \code{formula = y ~ x}, the default; (right)
# \code{formula = y ~ ns(x, 5)}.
library(splines)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), 
  method = "lm")
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), 
  method = "lm", formula = y ~ ns(x,5))

# Using jittering (left) and boxplots (right) to investigate the
# distribution of price per carat, conditional on colour.  As the
# colour improves (from left to right) the spread of values decreases,
# but there is little change in the centre of the distribution.
#jitter是点状的柱图，横坐标是类别变量，纵坐标是连续变量
qplot(color, price / carat, data = diamonds, geom = "jitter")
qplot(color, price / carat, data = diamonds, geom = "boxplot")

# Varying the alpha level.  From left to right: $1/5$, $1/50$, $1/200$.
# As the opacity decreases we begin to see where the bulk of the data
# lies.  However, the boxplot still does much better.
qplot(color, price / carat, data = diamonds, geom = "jitter",
 alpha = I(1 / 5))
qplot(color, price / carat, data = diamonds, geom = "jitter",
 alpha = I(1 / 50))
qplot(color, price / carat, data = diamonds, geom = "jitter",
 alpha = I(1 / 200))

# Displaying the distribution of diamonds.  (Left) \code{geom =
# "histogram"} and (right) \code{geom = "density"}.
qplot(carat, data = diamonds, geom = "histogram")
qplot(carat, data = diamonds, geom = "density")

# Varying the bin width on a histogram of carat reveals interesting
# patterns.  Binwidths from left to right: 1, 0.1 and 0.01 carats. Only
# diamonds between 0 and 3 carats shown.  xlim是指对x轴的limits,从0—3
qplot(carat, data = diamonds, geom = "histogram", binwidth = 1, 
  xlim = c(0,3))
qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.1,
  xlim = c(0,3))
qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.01,
  xlim = c(0,3))

# Mapping a categorical variable to an aesthetic will automatically
# split up the geom by that variable.  (Left) Density plots are
# overlaid and (right) histograms are stacked.
#color是变量，按照钻石color的类别来填充颜色
qplot(carat, data = diamonds, geom = "density", colour = color)
qplot(carat, data = diamonds, geom = "histogram", fill = color)

# Bar charts of diamond colour.  The left plot shows counts and the
# right plot is weighted by \code{weight = carat} to show the total
# weight of diamonds of each colour.,wigh
# weight是权重, scale_y_continuous("carat") 设定y的变量为carat
# 如果只放一个变量的话，默认的就是输出直方图，如果放两个则默认的是散点图
qplot(color, data = diamonds, geom = "bar")
qplot(color, data = diamonds, geom = "bar", weight = carat) +
  scale_y_continuous("carat")



library(readr)
ecinomics <- read_csv("C:\\Users\\Think\\Desktop\\ggplot2-master\\data-raw\\economics.csv")
library(ggplot2)

# Two time series measuring amount of unemployment.  (Left) Percent of
# population that is unemployed and (right) median number of weeks
# unemployed.  Plots created with {\tt geom="line"}.
qplot(date, unemploy / pop, data = economics, geom = "line")
qplot(date, uempmed, data = economics, geom = "line")

# Path plots illustrating the relationship between percent of people
# unemployed and median length of unemployment.  (Left) Scatterplot
# with overlaid path.  (Right) Pure path plot coloured by year.
year <- function(x) as.POSIXlt(x)$year + 1900
qplot(unemploy / pop, uempmed, data = economics, 
   geom = c("point", "path"))
qplot(unemploy / pop, uempmed, data = economics, 
  geom = "path", colour = year(date)) + scale_area()

# Histograms showing the distribution of carat conditional on colour.
# (Left) Bars show counts and (right) bars show densities (proportions
# of the whole).  The density plot makes it easier to compare
# distributions ignoring the relative abundance of diamonds within each
# colour. High-quality diamonds (colour D) are skewed towards small
# sizes, and as quality declines the distribution becomes more flat.
# row_variable ~ col_variable, 分面时用来规定行数和列数，如果只需要一行一列，用.占位即可
qplot(carat, data = diamonds, facets = color ~ ., 
  geom = "histogram", binwidth = 0.1, xlim = c(0, 3))
qplot(carat, ..density.., data = diamonds, facets = color ~ .,
  geom = "histogram", binwidth = 0.1, xlim = c(0, 3))


#xlab 和 ylab分别指定x和y轴的text标签部分，main 指定图头标题，log = ("xy")表示对xy都取对数
qplot(
  carat, price, data = dsmall, 
  xlab = "Price ($)", ylab = "Weight (carats)",  
  main = "Price-weight relationship"
)
qplot(
   carat, price/carat, data = dsmall, 
   ylab = expression(frac(price,carat)), 
   xlab = "Weight (carats)",  
   main="Small diamonds", 
   xlim = c(.2,1)
)
qplot(carat, price, data = dsmall, log = "xy")
