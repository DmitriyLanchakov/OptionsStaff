\name{gg}
\alias{gg}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("gg")}
\format{
  The format is:
List of 9
 $ data       :'data.frame':	51 obs. of  2 variables:
  ..$ ba: num [1:51] 35 35.1 35.2 35.3 35.4 35.5 35.6 35.7 35.8 35.9 ...
  ..$ pr: num [1:51] 0.0789 0.0763 0.0737 0.0711 0.0684 ...
 $ layers     :List of 1
  ..$ :Classes 'proto', 'environment' <environment: 0x000000002cd4add0> 
 $ scales     :Reference class 'Scales' [package "ggplot2"] with 1 fields
  ..$ scales:List of 1
  .. ..$ :List of 17
  .. .. ..$ call        : language continuous_scale(aesthetics = c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final"), scale_name = "position_c",      palette = identity, labels = ..1, expand = expand, guide = "none")
  .. .. ..$ aesthetics  : chr [1:7] "y" "ymin" "ymax" "yend" ...
  .. .. ..$ scale_name  : chr "position_c"
  .. .. ..$ palette     :function (x)  
  .. .. ..$ range       :Reference class 'Continuous' [package "scales"] with 1 fields
  .. .. .. ..$ range: NULL
  .. .. .. ..and 15 methods, of which 3 are possibly relevant:
  .. .. .. ..  initialize, reset, train
  .. .. ..$ limits      : NULL
  .. .. ..$ trans       :List of 6
  .. .. .. ..$ name     : chr "identity"
  .. .. .. ..$ transform:function (x)  
  .. .. .. ..$ inverse  :function (x)  
  .. .. .. ..$ breaks   :function (x)  
  .. .. .. ..$ format   :function (x)  
  .. .. .. ..$ domain   : num [1:2] -Inf Inf
  .. .. .. ..- attr(*, "class")= chr "trans"
  .. .. ..$ na.value    : num NA
  .. .. ..$ expand      : list()
  .. .. .. ..- attr(*, "class")= chr "waiver"
  .. .. ..$ rescaler    :function (x, to = c(0, 1), from = range(x, na.rm = TRUE))  
  .. .. ..$ oob         :function (x, range = c(0, 1), only.finite = TRUE)  
  .. .. ..$ name        : NULL
  .. .. ..$ breaks      : list()
  .. .. .. ..- attr(*, "class")= chr "waiver"
  .. .. ..$ minor_breaks: list()
  .. .. .. ..- attr(*, "class")= chr "waiver"
  .. .. ..$ labels      :function (x)  
  .. .. ..$ legend      : NULL
  .. .. ..$ guide       : chr "none"
  .. .. ..- attr(*, "class")= chr [1:3] "position_c" "continuous" "scale"
  ..and 21 methods, of which 9 are possibly relevant:
  ..  add, clone, find, get_scales, has_scale, initialize, input, n, non_position_scales
 $ mapping    :List of 2
  ..$ x: symbol ba
  ..$ y: symbol pr
 $ theme      :List of 1
  ..$ axis.text:List of 8
  .. ..$ family    : NULL
  .. ..$ face      : NULL
  .. ..$ colour    : chr "black"
  .. ..$ size      :Class 'rel'  num 0.8
  .. ..$ hjust     : NULL
  .. ..$ vjust     : NULL
  .. ..$ angle     : NULL
  .. ..$ lineheight: NULL
  .. ..- attr(*, "class")= chr [1:2] "element_text" "element"
  ..- attr(*, "class")= chr [1:2] "theme" "gg"
  ..- attr(*, "complete")= logi FALSE
 $ coordinates:List of 1
  ..$ limits:List of 2
  .. ..$ x: NULL
  .. ..$ y: NULL
  ..- attr(*, "class")= chr [1:2] "cartesian" "coord"
 $ facet      :List of 1
  ..$ shrink: logi TRUE
  ..- attr(*, "class")= chr [1:2] "null" "facet"
 $ plot_env   :<environment: R_GlobalEnv> 
 $ labels     :List of 2
  ..$ x: chr "Цена базисного актива, USDRUB"
  ..$ y: chr "Доходность, %"
 - attr(*, "class")= chr [1:2] "gg" "ggplot"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(gg)
## maybe str(gg) ; plot(gg) ...
}
\keyword{datasets}
