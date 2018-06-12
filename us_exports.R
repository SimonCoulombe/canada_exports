plotly_username = Sys.getenv("plotly_username")
plotly_api_key = Sys.getenv("plotly_api_key")
  

library(dplyr)
library(magrittr)
library(readr)
library(purrr)
library(plotly)
library(tidyr)

states <- c( "al", "ak", "az", "ar", "ca", "co", "ct", "de", "dc", "fl", "ga",
             "hi" , "id", "il", "in","ia",  "ks", "ky", "la" ,"me", "md",
             "ma", "mi",  "mn",  "ms", "mo", "mt", "ne", "nv", "nh", "nj", "nm",
             "ny", "nc", "nd", "oh", "ok", "or", "pa", "pr", "ri", "sc", "sd",
             "tn", "tx", "ut", "vt", "va", "vi", "wa", "wv", "wi",  "wy")
             

myfun <- function(state_2){
  download.file(
    url= paste0("https://www.census.gov/foreign-trade/statistics/state/data/excty", 
                state_2,".csv"),
    destfile = paste0("excty",state_2,".csv"))

exctyal <- 
  readr::read_csv(paste0("excty",state_2,".csv"), skip =5 ) %>%
  tail(-3) %>%
  mutate_at(vars( -countryd),
            funs(as.numeric(.)))  %>%
  mutate(share17 = round(share17, digits=1))


rank_canada <- 
  exctyal %>% 
  summarise(rank_canada = max(rank * (countryd=="Canada")),
            share_canada = max(share17 * (countryd == "Canada")) ) %>%
  mutate(rank_canada = ifelse(rank_canada>0, rank_canada, NA),
         share_canada = ifelse(share_canada>0, share_canada, NA)
         )

return <- exctyal  %>% 
  select(rank, countryd) %>%  
  spread(key=rank, value = countryd, sep = "_") %>% 
  bind_cols(
    exctyal %>% select(share=rank, share17) %>%  spread(key=share, value = share17, sep="_")) %>%
  mutate(CODE = toupper(state_2)) %>%
  bind_cols(rank_canada)
}
z  <- purrr::map(states, myfun)

zz <- z %>% bind_rows %>%
  mutate(hover = paste(CODE, "<br>", 
                       "Rank - Country - Share of exports <br>",
                       "1", rank_1, share_1,"<br>",
                       "2", rank_2, share_2,"<br>",
                       "3", rank_3, share_3,"<br>",
                       "4", rank_4, share_4,"<br>",
                       "5", rank_5, share_5,"<br>",
                       "6", rank_6, share_6,"<br>",
                       "7", rank_7, share_7,"<br>",
                       "8", rank_8, share_8,"<br>",
                       "9", rank_9, share_9,"<br>",
                       "10", rank_10, share_10,"<br>",
                       "11", rank_11, share_11,"<br>",
                       "12", rank_12, share_12,"<br>",
                       "13", rank_13, share_13,"<br>",
                       "14", rank_14, share_14,"<br>",
                       "15", rank_15, share_15,"<br>",
                       "16", rank_16, share_16,"<br>",
                       "17", rank_17, share_17,"<br>",
                       "18", rank_18, share_18,"<br>",
                       "19", rank_19, share_19,"<br>",
                       "20", rank_20, share_20)) %>%
  select(CODE, hover, rank_canada, share_canada, rank_1)

# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

p <- plot_geo(zz, locationmode = 'USA-states') %>%
  add_trace(
    z = ~rank_canada, text = ~hover, locations = ~CODE,
    reversescale = TRUE,
    color = ~rank_canada, colors = 'Purples'
  ) %>%
  colorbar(title = "Rank of Canada") %>%
  layout(
    title = "Rank of Canada among export trading partner, 2017 data",
    geo = g
  )

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="choropleth-exports")
chart_link
