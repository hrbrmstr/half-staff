library(here, include.only = "here")
library(rvest)
library(stringi)
library(tidyverse)

# Check for new (do this daily) --------

httr::GET("https://flagsexpress.com/flags-half-staff/?page=1") |>
  httr::content() |> 
  rvest::html_nodes("h3.blog-title > a") |> 
  rvest::html_attr("href") -> current_links

# Gather up old scraped

scraped_links <- read_lines(here::here("data", "scraped.txt"))

# Check for diffs --------

setdiff(
  current_links,
  scraped_links
) -> new_links

# Manual inspection of new links --------

new_links

if (length(new_links)) {
  
  new_links |> 
    map(~{
      
      rvest::read_html(.x) |> 
        rvest::html_node("div.blog-post") |> 
        html_text2()
      
    }) -> reasons
  
  new_links |> 
    map(~{
      
      rvest::read_html(.x) |> 
        rvest::html_node("h3.blog-title") |> 
        html_text(trim = TRUE) 
      
    }) -> titles
  
  new_links |> 
    map_df(~{
      
      meta <- gsub("^https://flagsexpress.com/flags-half-staff/|/$", "", .x)
      meta <- unlist(strsplit(meta, "/"))
      
      tibble(
        state = stri_trans_totitle(sub("-", " ", meta[1])),
        date_raw = meta[2],
        date = lubridate::mdy(meta[2]),
        url = .x
      )
      
    }) -> xdf
  
  xdf$title <- flatten_chr(ifelse(lengths(titles), titles, NA_character_))
  xdf$reason <- flatten_chr(reasons)
  
  # ——> Manual inspection of new record(s) --------
  
  xdf 
  
  PROCESS_NEW <- FALSE # makes sure automation doesn't kick in
  
  if (PROCESS_NEW) {
    
    # ——> Add new records) --------
    bind_rows(
      
      xdf |> 
        select(state, date, reason),
      
      read_csv(
        
        file = here::here("data", "latest.csv"),
        
        col_types = cols(
          state = col_character(),
          date = col_date(format = ""),
          reason = col_character()
        )
        
      )
      
    ) -> ydf
    
    # ——> Write out new records (CSV) --------
    write_csv(ydf, here::here("data", sprintf("%s-collection.csv", Sys.Date())))
    
    file.copy(
      from = here::here("data", sprintf("%s-collection.csv", Sys.Date())),
      to = here::here("data", "latest.csv"),
      overwrite = TRUE
    )
    
    # ——> Write out new records (JSON) --------
    jsonlite::stream_out(ydf, file(here::here("data", sprintf("%s-collection.json", Sys.Date()))))
    
    file.copy(
      from = here::here("data", sprintf("%s-collection.json", Sys.Date())),
      to = here::here("data", "latest.json"),
      overwrite = TRUE
    )
    
    # ——> Write out links --------
    writeLines(c(scraped_links, new_links), here::here("data", "scraped.txt"))
    
  } 
  
}

# Update RSS feed -----------------------------------------------------------------------------

read_csv(
  
  file = here::here("data", "latest.csv"),
  
  col_types = cols(
    state = col_character(),
    date = col_date(format = ""),
    reason = col_character()
  )
  
) |> 
  filter(
    lubridate::year(date) == lubridate::year(Sys.Date())
  ) |> 
  arrange(desc(date)) -> xdf

xdf$digest <- sapply(xdf$reason, digest::digest, algo = "sha256")

cleanup_description <- \(x) {
  gsub("(â|â|â|â¯|â )", "—", gsub("(â|â)", "'", gsub("â", '"', gsub("â", '"', stri_enc_toutf8(x)))))
}

xdf$reason |> 
  map_chr(~{
    paste0(
      { tmp <- stri_trim_both(unlist(stri_split_lines(cleanup_description(.x))))
        tmp[tmp == ""] <- "<br/>"
        tmp
      }, 
      collapse = "\n")
  }) -> xdf$description 

cat(
  '<?xml version="1.0" ?>
<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
<channel>
  <title>U.S. Flag Half-staff Declarations</title>
  <link>https://hrbrmstr.github.io/half-staff/</link>
  <description>U.S. Flag Half-staff Declarations</description>
  <atom:link href="https://hrbrmstr.github.io/half-staff/" rel="self" type="application/rss+xml" />
  <image>
    <title>U.S. Flag Half-staff Declarations</title>
    <url>https://hrbrmstr.github.io/half-staff/half-staff-flag.png</url>
    <link>https://hrbrmstr.github.io/half-staff/</link>
  </image>
',
  file = here::here("docs", "index.rss")
)

cat(
  sprintf(
    '<item>
<title>%s (%s)</title>
<link>https://hrbrmstr.github.io/half-staff/</link>
<guid>https://hrbrmstr.github.io/half-staff/%s</guid>
<description>
<![CDATA[
%s
]]>
</description>
</item>
',
    xdf$date,
    xdf$state, 
    xdf$digest,
    xdf$description    
  ),
  sep = "\n",
  file = here::here("docs", "index.rss"),
  append = TRUE
)

cat(
  '</channel>
</rss> 
',
  file = here::here("docs", "index.rss"),
  append = TRUE
)

