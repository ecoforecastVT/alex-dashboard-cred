library(whisker)
library(httr)

config <- yaml::read_yaml('challenge_configuration.yaml')

links <- c(paste0(config$challenge_url, "/water.html"),
           paste0(config$challenge_url, "/weather.html"),
           paste0(config$challenge_url, "/index.html"))

map_links <- function(l) {
  tmp <- GET(l)
  d <- tmp$headers[['last-modified']]
  
  list(loc=l,
       lastmod=format(as.Date(d,format="%a, %d %b %Y %H:%M:%S")))
}

links <- lapply(links, map_links)

cat(whisker.render(tpl), file = "dashboard/docs/sitemap.xml")
