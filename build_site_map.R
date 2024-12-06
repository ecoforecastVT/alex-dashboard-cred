library(whisker)
library(httr)

#config <- yaml::read_yaml('challenge_configuration.yaml')

tpl <- '<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
 {{#links}}
   <url>
      <loc>{{{loc}}}</loc>
      <lastmod>{{{lastmod}}}</lastmod>
   </url>
 {{/links}}
</urlset>'

links <- c(paste0('https://flare-forecast.org', "/water.html"),
           paste0('https://flare-forecast.org', "/weather.html"),
           paste0('https://flare-forecast.org', "/index.html"))

map_links <- function(l) {
  tmp <- httr::GET(l)
  d <- tmp$headers[['last-modified']]
  
  list(loc=l,
       lastmod=format(as.Date(d,format="%a, %d %b %Y %H:%M:%S")))
}

links <- lapply(links, map_links)

cat(whisker.render(tpl), file = "docs/sitemap.xml")
