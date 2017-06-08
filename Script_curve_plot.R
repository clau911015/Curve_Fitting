ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE, 
                     repos='http://cran.us.r-project.org')
  sapply(pkg, require, character.only = TRUE)
}

packages<-c("plotly","dplyr","tidyr","purrr","quantmod","magrittr")

ipack(packages)


# get yields from St. Louis Fed FRED
yield_curve <- list("DTB3", "DGS2", "DGS5", "DGS10", "DGS30") %>%
  map(
    ~getSymbols(.x, auto.assign=FALSE, src="FRED")
  ) %>%
  do.call(merge,.)

# create our 3d surface yield curve
yield_curve["1980::"] %>%
  # convert to numeric matrix
  data.matrix() %>% 
  # transpose
  t() %>%
  # draw our Plotly 3d surface
  plot_ly(
    x=as.Date(index(yield_curve["1980::"])),
    y=c(0.25,2,5,10,30),
    z=.,
    type="surface"
  ) %>%
  plotly::layout(
    scene=list(
      xaxis=list(title="date"),
      yaxis=list(title="term"),
      zaxis=list(title="yield")
    )
  )


# 3d scatter chart
yield_curve_tidy <- yield_curve %>%
  data.frame() %>%
  add_rownames(var="date") %>%
  
  gather(symbol,yield,-date) %>%
  mutate(term=c(0.25,5,10,30)[match(symbol,paste0(c("IRX","FVX","TNX","TYX"),".Close"))])

yield_curve_tidy %>%
  plot_ly(
    x=date, y=yield, z=term,
    group=symbol, type="scatter3d"
  )
