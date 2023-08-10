tstamp <- function(year=TRUE, month=TRUE, day=TRUE, 
                   hour=FALSE, minute=FALSE, second=FALSE) {
  stamp1 <- c()
  stamp2 <- c()
  if (year & !month & !day) {
    stamp <- format(Sys.time(), "%Y")  
  } else if (year & month & !day) {
    stamp1 <- format(Sys.time(), "%Y%m")
  } else if (year & month & day) {
    stamp1 <- format(Sys.time(), "%Y%m%d")
  } else if (!year & month & day) {
    stamp1 <- format(Sys.time(), "%m%d")
  } else if (year & !month & day) {
    stamp1 <- format(Sys.time(), "%Y%d")
  } else if (!year & month & !day) {
    stamp1 <- format(Sys.time(), "%m")
  } else if (!year & !month & day) {
    stamp1 <- format(Sys.time(), "%d")
  } else{ stamp1 <- "You'd better select parameters well."}
  
  if (hour & !minute & !second) {
    stamp2 <- format(Sys.time(), "%H")  
  } else if (hour & minute & !second) {
    stamp2 <- format(Sys.time(), "%H%M")
  } else if (hour & minute & second) {
    stamp2 <- format(Sys.time(), "%H%M%S")
  } else if (!hour & minute & !second) {
    stamp2 <- format(Sys.time(), "%M")
  } else if (!hour & !minute & second) {
    stamp2 <- format(Sys.time(), "%S")
  } else if (!hour & minute & second) {
    stamp2 <- format(Sys.time(), "%M%S")
  } else{}
  
  if (!is.null(stamp2)) {
    stamp1 <- paste0(stamp1, "T", stamp2)
  }
  return (stamp1)
}


#' Clean and standardize country names according to VIMC report templates
#'
#' The \code{clean_country_names()} is used to clean and standardize country names according to VIMC report templates
#' population for both sexes and incidence rate
#' @param country A vector of country names in character
#' @export
#' @examples
#' country <- clean_country_names(country = "DRC")
#' # Congo, the Democratic Republic of the
clean_country_names <- function(country) {
  for (i in 1:length(country)) {
    if (country[i] %in% c("DR Congo", "Democratic Republic of the Congo", "DRC", "Congo, Dem. Rep.", "Congo, DR",  "Congo, the Democratic Republic of the")){
      country[i] <- "Congo, Democratic Republic of the"
    }
    if (country[i] %in% c("Congo, Rep.", "Republic of the Congo", "Congo")){
      country[i] <- "Congo, Republic of the"
    }
    if (country[i] %in% c("São Tomé and Príncipe")){
      country[i] <- "Sao Tome e Principe"
    }
    if (country[i] %in% c("Iran", "Iran, Islamic Rep.", "Iran (Islamic Republic of)")){
      country[i] <- "Iran, Islamic Republic of"
    }
    if (country[i] %in% c("North Korea", "Korea:North", "Korea, DPR", "DPRK", "Democratic People's Republic of Korea", "Korea DPR")){
      country[i] <- "Korea, Democratic People's Republic of"
    }
    if (country[i] %in% c("South Korea", "Korea:South", "Korea, Rep.")){
      country[i] <- "Korea, the Republic of"
    }
    if (country[i] %in% c("Sudan: South")){
      country[i] <- "South Sudan"
    }
    if (country[i] %in% c("Sudan: North")){
      country[i] <- "Sudan"
    }
    if (country[i] %in% c("Venezuela", "Venezuela, RB", "Venezuela (Bolivarian Republic of)")){
      country[i] <- "Venezuela, Bolivarian Republic of"
    }
    if (country[i] %in% c("Tanzania", "United Republic of Tanzania")){
      country[i] <- "Tanzania, United Republic of"
    }
    if (country[i] %in% c("Syria")){
      country[i] <- "Syrian Arab Republic"
    }
    if (country[i] %in% c("Moldova", "Republic of Moldova")){
      country[i] <- "Moldova, Republic of"
    }
    if (country[i] %in% c("CAR")){
      country[i] <- "Central African Republic"
    }
    if (country[i] %in% c("Lao", "Laos", "Lao PDR")){
      country[i] <- "Lao People's Democratic Republic"
    }
    if (country[i] %in% c("US", "USA")){
      country[i] <- "United States of America"
    }
    if (country[i] %in% c("C?te d'Ivoire", "CÃ´te d'Ivoire", "Cì²™te d'Ivoire", "Côte d'Ivoire")){
      country[i] <- "Cote d'Ivoire"
    }
    if (country[i] %in% c("Bolivia", "Bolivia (Plurinational State of)")){
      country[i] <- "Bolivia, Plurinational State of"
    }
    if (country[i] %in% c("Cape Verde")){
      country[i] <- "Cabo Verde"
    }
    if (country[i] %in% c("Micronesia", "Micronesia (Federated States of)")){
      country[i] <- "Micronesia, Federated States of"
    }
    if (country[i] %in% c("Sao Tome e Principe")){
      country[i] <- "Sao Tome and Principe"
    }
    if (country[i] %in% c("Vietnam")){
      country[i] <- "Viet Nam"
    }
    if (country[i] %in% c("Eswatini")){ # to be consistent with other data files
      country[i] <- "Swaziland"
    }
  }
  return (country)
}



logistic <- function(L = 1, x, x0, k){
  return (L/(1 + exp(-k*(x-x0))))
}


theme_pub <- function( base_size=10, base_family="sans" ) {
    library(grid)
    library(ggthemes)
    ( theme_foundation( base_size=base_size, base_family=base_family )
        + theme( 
            plot.title = element_text( face="bold", size=rel(1.2), hjust=0.5 ),
            text = element_text(),
            panel.background = element_rect( colour=NA ),
            plot.background = element_rect( colour=NA ),
            panel.border = element_rect( colour=NA ),
            axis.title = element_text( face="plain", size=rel(1) ),
            axis.title.y = element_text( angle=90, vjust=2 ),
            axis.title.x = element_text( vjust=-0.1 ),
            axis.text = element_text(), 
            axis.line = element_line( colour="black" ),
            axis.ticks = element_line(),
            panel.grid.major = element_line( colour="#f0f0f0" ),
            panel.grid.minor = element_blank(),
            legend.key = element_rect( fill="transparent", colour=NA ),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size = unit( 2, "mm" ),
            legend.spacing = unit( 0.2, "mm" ),
            legend.margin = margin( 0, 0, 0, 0, unit="mm" ),
            legend.title = element_text( face="plain" ),
            legend.box.background = element_rect( fill="transparent", colour=NA ),
            legend.background = element_rect( fill="transparent", colour=NA ),
            plot.margin = unit( c(1,1,1,1), "mm" ),
            strip.background = element_rect( colour="#f0f0f0", fill="#f0f0f0" ),
            strip.text = element_text( face="bold" )
        ) )
}

## custom theme for plotting a map
theme_map <- function() {
  theme( 
    plot.background = element_blank(),
    panel.background = element_blank(), # bg of the panel
    legend.background = element_blank(), # get rid of legend bg
    legend.box.background = element_blank(),
    panel.spacing = unit( c(0,0,0,0), "null" ),
    plot.margin = unit( c(0,0,0,0), "null" ),
    axis.line = element_blank(), 
    axis.text.x = element_blank(),
    axis.text.y = element_blank(), 
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.25,0.38) )  
}

scale_fill_pub <- function(...){
    library(scales)
    discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
    
}

scale_colour_pub <- function(...){
    library(scales)
    discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
    
}