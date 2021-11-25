library(magrittr)
library(lattice)
# armd -> longitudinal (long) format, baseline information (week0) is removed
#         in the levels of the variable treat.f of armd data.frame.
# armd.wide -> wide format
data(armd, package = "nlmeU")
data(armd0, package = "nlmeU") 
data(armd.wide, package = "nlmeU")

# Names of factors variables
facs <- sapply(armd.wide, is.factor)
facs[facs] |> names()

facs <- sapply(armd, is.factor)
facs[facs] |> names()

levels(armd$treat.f)

# ------------------------------------------------------------------------------
# Evolution of the visual accuity by measurement date without dealing with NA
# ------------------------------------------------------------------------------

# By individuals
# ---------------

# tidyverse way
armd0 %>% 
  ggplot2::ggplot(ggplot2::aes(x = time, y = visual, group = subject)) +
  ggplot2::geom_line() +
  ggplot2::scale_x_continuous(breaks = unique(armd0$time)) +
  ggplot2::facet_grid(~treat.f) +
  ggplot2::theme(
    legend.position = "none"
  )

# Base way
xyplot(visual ~ time | treat.f, 
       data = armd0,
       xlim = unique(armd0$time),
       type = 'b'
       )



# Mean evolution
# --------------
   
# tidyverse way
armd0 %>% 
  dplyr::group_by(treat.f, time) %>% 
  dplyr::summarise(
    visual_mean = mean(visual)
  ) %>% 
  ggplot2::ggplot(ggplot2::aes(x = time, y = visual_mean, color = treat.f)) +
  ggplot2::geom_line() +
  ggplot2::scale_x_continuous(breaks = unique(armd0$time))

# Base way
grp <- with(armd0, list(time.f, treat.f))
tapply(armd0$visual, grp, FUN = mean) 