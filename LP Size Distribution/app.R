library(shiny)
library(shinythemes)
library(RColorBrewer)
library(plotly)
library(scales)
library(reactable)
library(tidyverse)


#Get data
lp_positions <- jsonlite::fromJSON("https://api.flipsidecrypto.com/api/v2/queries/cbfc16ba-5476-4816-833c-4b7d8a44dfba/data/latest") %>% 
  as_tibble(.) 

pool_liquidity_usd <- jsonlite::fromJSON("https://api.flipsidecrypto.com/api/v2/queries/63b49f65-de05-4019-859a-d426a1e8f7df/data/latest") %>% 
  as_tibble(.) %>%
  mutate(DAY = as.Date(DAY, "%Y-%m-%d")) %>%
  mutate(POOL_NAME_SHORT = str_replace(POOL_NAME, "-[:graph:]+$",""),
         CHAIN = str_extract(POOL_NAME_SHORT, "^[:alpha:]+(?=\\.)"))
  

#Prepare
lp_positions_usd <- lp_positions %>%
  filter(LP_POSITION >0 ) %>%
  left_join(., pool_liquidity_usd %>%
              select(POOL_NAME, TOTAL_LIQUIDITY_USD), by = "POOL_NAME") %>%
  group_by(POOL_NAME) %>%
  mutate(TOTAL_LP_POSITION = sum(LP_POSITION),
         LP_POSITION_USD = LP_POSITION/TOTAL_LP_POSITION * TOTAL_LIQUIDITY_USD) %>%
  ungroup() %>%
  mutate(POOL_NAME_SHORT = str_replace(POOL_NAME, "-[:graph:]+$",""),
         CHAIN = str_extract(POOL_NAME_SHORT, "^[:alpha:]+(?=\\.)"))

#Top 10 pools by TVL
pools_top10 <- lp_positions_usd %>%
  group_by(POOL_NAME_SHORT) %>%
  summarise(LP_POSITION_USD = sum(LP_POSITION_USD)) %>%
  filter(LP_POSITION_USD >0) %>%
  arrange(desc(LP_POSITION_USD)) %>%
  slice_head(., n = 10) %>%
  arrange(POOL_NAME_SHORT)

pools_others <- lp_positions_usd %>%
  group_by(POOL_NAME_SHORT) %>%
  summarise(LP_POSITION_USD = sum(LP_POSITION_USD)) %>%
  filter(LP_POSITION_USD >0) %>%
  anti_join(., pools_top10,  by = c("POOL_NAME_SHORT", "LP_POSITION_USD")) %>%
  arrange(POOL_NAME_SHORT)

#Shiny

ui <- tagList(
 # shinythemes::themeSelector(),
  navbarPage( "LP Size Distribution",
               theme = "spacelab",  # <--- To use a theme, uncomment this
                  tabPanel("About this dahboard",
                           markdown(
                            " # LP Size Distribution
This interactive dashboard was built as a submission to Flipsidecrypto's Thorchain LP Size Distribution bounty. Given the descriptive nature of the bounty, which merely asked to show the liquidity positions distribution of the different Thorchain pools, and the large number of pools, we decided to build a tool that empowers users to explore, and even compare, the LP distribution of the different chains.
  
  

## Dashboard documentation

This dashboard has three different segments (excluding this introductory and explanatory one):  


#### Liquidity Summary
This section, the only non-interactive one, summarises liquidity by pool and by chain. It shows a histogram and a cumulative distribution graph of the TVL (total value pooled) in USD of all pools first, and all chains later. It also shows a summary table both for pools and for chains.  


#### Analyze pools
It allows the user to analyze the LP distribution of any pool or combination of pools. Basically, when a user selects more than one pool, the plots take into consideration all liquidity positions (in USD) on the selected pools and show their distribution.  


#### Compare pools
It allows the user to compare the LP distribution of any combination of pools. Contrary to the previous section, when a user selects more than one pool, the plots show the LP distribution of each pool superimposed on each other, to allow for a visual comparison. For this reason, we recommend not selecting too many pools at once, as the plots become ever more difficult to analyze.  


#### Compare chains
Similar to the previous section, but allows users to compare total liquidity positions by chain.  


## Data and methodology
#### Data and reproducibility
All data used on this dashboard is updated daily and comes from Flipsidecrypto's thorchain.liquidity_actions and thorchain.daily_pool_stats tables. In the spirit of reproducibility, queries used to extract the data can be found [here](https://app.flipsidecrypto.com/velocity/queries/cbfc16ba-5476-4816-833c-4b7d8a44dfba) and [here](https://app.flipsidecrypto.com/velocity/queries/63b49f65-de05-4019-859a-d426a1e8f7df). The code underlying this app can be found [here]().
                             
#### Methodology
The first step to build this app, and to answer the bounty for what it matters, relied on estimating wallets' liquidity providing positions on each pool.

In order to do so, we calculated the deposits'  and withdrawal's amount (in staking units) that each wallet made into each pool and netted them out.

Then we estimated each wallet's LP in USD by multiplying its share of the pool's total staking units by the total USD value of the pooled assets into it.

An important consideration is that we excluded some positions that showed negative LP, that is, their total withdrawals (in staking units) exceeded their deposits: as we see it, there are two possibilities for this: 1)  a data failure that was translated into some deposits being missed, 2) onde wallet transferring claiming tokens to another, which later withdrew liquidity.

In either case, our methodology should produce good estimates of the LP distribution under the assumption that any missing data points have an equal underlying distribution, given that our sample is large enough (and even more so given that the missing data points should be a small proportion of the overall universe)."

                           )
                  ),
              tabPanel("Liquidity summary",
                       plotlyOutput("plotSum1"),
                       plotlyOutput("plotSum2"),
                       reactableOutput("tableSum1"),
                       plotlyOutput("plotSum3"),
                       plotlyOutput("plotSum4"),
                       reactableOutput("tableSum2"),
              ),
                  tabPanel("Analyze Pools",
                  sidebarLayout(
                  sidebarPanel(
                    checkboxGroupInput('pools_1', 'Pool(s) to include (top 10)', choices = pools_top10 %>%
                                         select(POOL_NAME_SHORT) %>%
                                         pull(.),
                                       selected = pools_top10 %>%
                                         slice_max(., LP_POSITION_USD, n = 1) %>%
                                         select(POOL_NAME_SHORT) %>%
                                         pull(.)),
                    checkboxGroupInput('pools_1.1', 'Pool(s) to include (others)', choices = pools_others %>%
                                         select(POOL_NAME_SHORT) %>%
                                         pull(.)
                                       )
                    ),
                  mainPanel(
                    plotlyOutput("plot1"),
                    plotlyOutput("plot2"),
                    plotlyOutput("plot3")
                  ))
                  ),
                  tabPanel("Compare Pools",
                           sidebarLayout(
                             sidebarPanel(
                               checkboxGroupInput('pools_2', 'Pool(s) to compare (top 10)', choices = pools_top10 %>%
                                                    select(POOL_NAME_SHORT) %>%
                                                    pull(.),
                                                  selected = pools_top10 %>%
                                                    slice_max(., LP_POSITION_USD, n = 2) %>%
                                                    select(POOL_NAME_SHORT) %>%
                                                    pull(.)),
                               checkboxGroupInput('pools_2.1', 'Pool(s) to compare (others)', choices = pools_others %>%
                                                    select(POOL_NAME_SHORT) %>%
                                                    pull(.)
                               )
                               ),
                             mainPanel(
                               plotlyOutput("plot4"),
                               plotlyOutput("plot5"),
                               plotlyOutput("plot6")
                             ))
                  ),
              tabPanel("Compare Chains",
                       sidebarLayout(
                         sidebarPanel(
                           checkboxGroupInput('chains', 'Chains(s) to compare', choices = pool_liquidity_usd %>%
                                                distinct(CHAIN) %>%
                                                arrange(CHAIN) %>%
                                                pull(.),
                                              selected = pool_liquidity_usd %>%
                                                group_by(CHAIN) %>%
                                                summarise(TOTAL_LIQUIDITY_USD = sum(TOTAL_LIQUIDITY_USD)) %>%
                                                arrange(desc(TOTAL_LIQUIDITY_USD)) %>%
                                                slice_max(., TOTAL_LIQUIDITY_USD, n = 2) %>%
                                                select(CHAIN) %>%
                                                pull(.))
                         ),
                         mainPanel(
                           plotlyOutput("plot7"),
                           plotlyOutput("plot8"),
                           plotlyOutput("plot9")
                         ))
              )
                  
                  )
)

server <- function(input, output) {
  ### Pool TVL
  #Histogram (by pool)
  output$plotSum1 <-  renderPlotly({
    p1 <-  pool_liquidity_usd %>%
      filter(TOTAL_LIQUIDITY_USD >0) %>%
      ungroup() %>%
      ggplot(.,aes(x=TOTAL_LIQUIDITY_USD, text = paste0("[",scales::dollar(10^(..xmin..)),",",scales::dollar(10^(..xmax..)),"] count: ",..count..))) + 
      geom_histogram(bins = 10,  fill = RColorBrewer::brewer.pal(3,"Set3")[[1]] ) +
      scale_x_continuous(trans = "log10", labels = label_comma()) +
      ggtitle("Pool TVL: histogram") +
      xlab("TVL (USD)") + 
      ylab("Count")
    ggplotly(p1, tooltip = "text")
  }) 
  
  # Cum dist (by pool)
  output$plotSum2 <-  renderPlotly({
    p2 <- pool_liquidity_usd %>%
      filter(TOTAL_LIQUIDITY_USD >0) %>%
      ungroup() %>%
      arrange(TOTAL_LIQUIDITY_USD) %>%
      ggplot(., aes(x = TOTAL_LIQUIDITY_USD, text = paste0("TVL: ",scales::dollar(10^(..x..)),"<br>",
                                                           "Cumulative prob.: ",round(..y.., 4)))) +
      stat_ecdf(pad = FALSE,   color = RColorBrewer::brewer.pal(3,"Set3")[[1]]) +
      scale_x_continuous(trans = "log10", labels = label_comma()) +
      ggtitle("Pool TVL: cumulative dist.") +
      xlab("TVL (USD)") + 
      ylab("Cumulative dist.")
    ggplotly(p2, tooltip = "text")
  }) 
  
  # Table (by pool)
  output$tableSum1 <- renderReactable({
    lp_positions_usd %>%
      group_by(POOL_NAME_SHORT, CHAIN) %>%
      summarise(LIQUIDITY_PROVIDERS = n(),
                TVL = sum(LP_POSITION_USD), .groups = "drop") %>%
      mutate(RANK = row_number(desc(TVL))) %>%
      arrange(desc(TVL)) %>%
      reactable::reactable(., columns = list(
        TVL = colDef(
          format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
        POOL_NAME_SHORT = colDef(
          name = "Pool"),
        CHAIN = colDef(
          name = "Chain"),
        LIQUIDITY_PROVIDERS = colDef(
          name = "Liquidity Providers",
          format = colFormat( separators = TRUE, digits = 0)
        ),
        RANK = colDef(
          name = "Rank")
      ),
      searchable = TRUE)
  })
  
  #Histogram (by chain)
  output$plotSum3 <-  renderPlotly({
    p1 <-  pool_liquidity_usd %>%
      group_by(CHAIN) %>%
      summarise(TOTAL_LIQUIDITY_USD = sum(TOTAL_LIQUIDITY_USD)) %>%
      filter(TOTAL_LIQUIDITY_USD >0) %>%
      ungroup() %>%
      ggplot(.,aes(x=TOTAL_LIQUIDITY_USD, text = paste0("[",scales::dollar(10^(..xmin..)),",",scales::dollar(10^(..xmax..)),"] count: ",..count..))) + 
      geom_histogram(bins = 5,  fill = RColorBrewer::brewer.pal(3,"Set3")[[1]] ) +
      scale_x_continuous(trans = "log10", labels = label_comma()) +
      ggtitle("Chain TVL: histogram") +
      xlab("TVL (USD)") + 
      ylab("Count")
    ggplotly(p1, tooltip = "text")
  }) 
  
  # Cum dist (by chain)
  output$plotSum4 <-  renderPlotly({
    p2 <-  pool_liquidity_usd %>%
      group_by(CHAIN) %>%
      summarise(TOTAL_LIQUIDITY_USD = sum(TOTAL_LIQUIDITY_USD)) %>%
      filter(TOTAL_LIQUIDITY_USD >0) %>%
      ungroup() %>%
      arrange(TOTAL_LIQUIDITY_USD) %>%
      ggplot(., aes(x = TOTAL_LIQUIDITY_USD, text = paste0("TVL: ",scales::dollar(10^(..x..)),"<br>",
                                                           "Cumulative prob.: ",round(..y.., 4)))) +
      stat_ecdf(pad = FALSE,   color = RColorBrewer::brewer.pal(3,"Set3")[[1]]) +
      scale_x_continuous(trans = "log10", labels = label_comma()) +
      ggtitle("Chain TVL: cumulative dist.") +
      xlab("TVL (USD)") + 
      ylab("Cumulative dist.")
    ggplotly(p2, tooltip = "text")
  }) 
  
  # Table (by chain)
  output$tableSum2 <- renderReactable({
    lp_positions_usd %>%
      group_by(CHAIN) %>%
      summarise(LIQUIDITY_PROVIDERS = n(),
                TVL = sum(LP_POSITION_USD, na.rm = TRUE), .groups = "drop") %>%
      mutate(RANK = row_number(desc(TVL))) %>%
      arrange(desc(TVL)) %>%
      reactable::reactable(., columns = list(
        TVL = colDef(
          format = colFormat(prefix = "$", separators = TRUE, digits = 0)),
        CHAIN = colDef(
          name = "Chain"),
        LIQUIDITY_PROVIDERS = colDef(
          name = "Liquidity Providers",
          format = colFormat( separators = TRUE, digits = 0)
        ),
        RANK = colDef(
          name = "Rank")
      ),
      searchable = TRUE)
  })
  
  ###Pool analize
  filterPools1 <- reactive({
    pools_1 <- c(input$pools_1, input$pools_1.1)
    pool_1_selected <- if(length(pools_1) > 0) {TRUE} else {FALSE}
    req(pool_1_selected)
    lp_positions_usd %>%
      filter(POOL_NAME_SHORT %in% pools_1 & LP_POSITION_USD >0 )
  })
  
  #Histogram
  output$plot1 <-  renderPlotly({
    p1 <- ggplot(filterPools1(),aes(x=LP_POSITION_USD, text = paste0("[",scales::dollar(10^(..xmin..)),",",scales::dollar(10^(..xmax..)),"] count: ",..count..))) + 
      geom_histogram(bins = 20, fill = RColorBrewer::brewer.pal(3,"Set3")[[1]] ) +
      scale_x_continuous(trans = "log10", labels = label_comma()) +
      ggtitle(if_else(length(input$pools_1) != 1,"Selected pool(s): histogram of LPs sizes",paste0(input$pools_1[[1]],": log-histogram of LPs sizes"))) +
      xlab("LP (USD)") + 
      ylab("Count") 
    ggplotly(p1, tooltip = "text")
  })
  
  #Density
  output$plot2 <-  renderPlotly({
    p2 <- ggplot(filterPools1(), aes(x= LP_POSITION_USD, text = paste0("Position: ",scales::dollar(10^(..x..)),"<br>", "Density: ",round(..density.., 2)))) + 
      geom_density(alpha=0.7, color = RColorBrewer::brewer.pal(3,"Set3")[[1]] , fill =RColorBrewer::brewer.pal(3,"Set3")[[1]] )+
      scale_x_continuous(trans = "log10", labels = label_comma()) +
      ggtitle(if_else(length(input$pools_1) != 1,"Selected pool(s): density dist. of LPs sizes",paste0(input$pools_1[[1]],": log-density dist. of LPs sizes"))) +
      xlab("LP size (USD)") + 
      ylab("Density")
    ggplotly(p2, tooltip = "text")
  })
  
  # Cumulative
  output$plot3 <-  renderPlotly({
      p1 <- filterPools1() %>% 
        arrange(LP_POSITION_USD) %>%
        ggplot(., aes(x = LP_POSITION_USD, text = paste0("Position: ",scales::dollar(10^(..x..)),"<br>",
                                                         "Cumulative prob.: ",round(..y.., 4)))) +
        stat_ecdf(pad = FALSE, color = RColorBrewer::brewer.pal(12,"Set3")[[1]] ) +
        scale_x_continuous(trans = "log10", labels = label_comma()) +
        ggtitle(if_else(length(input$pools_2) != 1,"Selected pool(s): cumulative distribution LPs sizes",paste0(input$pools_2[[1]],": log cumulative distribution LPs sizes"))) +
        xlab("LP size (USD)") + 
        ylab("Cumulative prob.")
      ggplotly(p1, tooltip = "text")
  })
  
  ###Pools compare
  filterPoolsCompare <- reactive({
    pools_2 <- c(input$pools_2, input$pools_2.1)
    pool_2_selected <- if(length(pools_2) > 0) {TRUE} else {FALSE}
    req(pool_2_selected)
    lp_positions_usd %>%
      filter(POOL_NAME_SHORT %in% pools_2 & LP_POSITION_USD >0 )
  })
  
  # Histogram
  output$plot4 <-  renderPlotly({
    p0 <-   ggplot(filterPoolsCompare(),aes(x=LP_POSITION_USD, fill = POOL_NAME_SHORT, text = paste0(..fill.., "<br>",
                                                                            "[",scales::dollar(10^(..xmin..)),",",scales::dollar(10^(..xmax..)),"] count: ",..count..))) + 
      scale_fill_manual(values = RColorBrewer::brewer.pal(12,"Set3"), name = "Pool") +
      geom_histogram(bins = 20,alpha=0.7 ,position="identity") +
      scale_x_continuous(trans = "log10", labels = label_comma()) +
      ggtitle(if_else(length(input$pools_2) != 1,"Selected pool(s): histogram of LPs sizes",paste0(input$pools_2[[1]],": histogram of LPs sizes"))) +
      xlab("LP size (USD)") + 
      ylab("Count")
    
   p1 <- ggplotly(p0, tooltip = "text")
        for (i in 1:length(p1$x$data)) {
      
      p1$x$data[[i]]$name <- p1$x$data[[i]]$name %>%
        str_replace(.,"^\\(","") %>%
        str_replace(., ",1\\)","" )
    }
    p1
  })
  
  # Density
  output$plot5 <-  renderPlotly({
    p0 <- ggplot(filterPoolsCompare(), aes(x= LP_POSITION_USD, colour=POOL_NAME_SHORT, fill=POOL_NAME_SHORT, text = paste0(..fill..,"<br>",
                                                                                        "Position: ",scales::dollar(10^(..x..)),"<br>",
                                                                                        "Density: ",round(..density.., 2)))) + 
      scale_fill_manual(values = RColorBrewer::brewer.pal(12,"Set3"), name = "Pool") +
      scale_color_manual(values = RColorBrewer::brewer.pal(12,"Set3"), name = "Pool") +
      geom_density(alpha=0.5 )+
      scale_x_continuous(trans = "log10", labels = label_comma()) +
      ggtitle(if_else(length(input$pools_2) != 1,"Selected pool(s): density dist. of LPs sizes",paste0(input$pools_2[[1]],": density dist. of LPs sizes"))) +
      xlab("LP size (USD)") + 
      ylab("Density")
    ggplotly(p0, tooltip = "text")
    })
  

  # Cumulative
  output$plot6 <-  renderPlotly({
    p1 <- filterPoolsCompare() %>% 
      arrange(LP_POSITION_USD) %>%
      ggplot(., aes(x = LP_POSITION_USD, color = POOL_NAME_SHORT,  text = paste0(..color..,"<br>",
                                                                           "Position: ",scales::dollar(10^(..x..)),"<br>",
                                                                           "Cumulative prob.: ",round(..y.., 4)))) +
      stat_ecdf(pad = FALSE) +
      scale_color_manual(values = RColorBrewer::brewer.pal(12,"Set3"), name = "Pool") +
      scale_x_continuous(trans = "log10", labels = label_comma()) +
      ggtitle(if_else(length(input$pools_2) != 1,"Selected pool(s): cumulative distribution LPs sizes",paste0(input$pools_2[[1]],": cumulative distribution LPs sizes"))) +
      xlab("LP size (USD)") + 
      ylab("Cumulative prob.")
    ggplotly(p1, tooltip = "text")
  })
  
  ###Chains compare
  filterChainsCompare <- reactive({
    chains_1 <- c(input$chains)
    chains_selected <- if(length(chains_1) > 0) {TRUE} else {FALSE}
    req(chains_selected)
    lp_positions_usd %>%
      filter(CHAIN %in% chains_1 & LP_POSITION_USD >0 )
  })
  
  
  # Histogram
  output$plot7 <-  renderPlotly({
    p0 <-   ggplot(filterChainsCompare(),aes(x=LP_POSITION_USD, fill = CHAIN, text = paste0(..fill.., "<br>",
                                                                                                     "[",scales::dollar(10^(..xmin..)),",",scales::dollar(10^(..xmax..)),"] count: ",..count..))) + 
      scale_fill_manual(values = RColorBrewer::brewer.pal(12,"Set3"), name = "Chain") +
      geom_histogram(bins = 20,alpha=0.7 ,position="identity") +
      scale_x_continuous(trans = "log10", labels = label_comma()) +
      ggtitle(if_else(length(input$chains) != 1,"Selected chain(s): histogram of LPs sizes",paste0(input$chains[[1]],": histogram of LPs sizes"))) +
      xlab("LP size (USD)") + 
      ylab("Count")
    
    p1 <- ggplotly(p0, tooltip = "text")
    for (i in 1:length(p1$x$data)) {
      
      p1$x$data[[i]]$name <- p1$x$data[[i]]$name %>%
        str_replace(.,"^\\(","") %>%
        str_replace(., ",1\\)","" )
    }
    p1
  })
  
  # Density
  output$plot8 <-  renderPlotly({
    p0 <- ggplot(filterChainsCompare(), aes(x= LP_POSITION_USD, colour=CHAIN, fill=CHAIN, text = paste0(..fill..,"<br>",
                                                                                                                           "Position: ",scales::dollar(10^(..x..)),"<br>",
                                                                                                                           "Density: ",round(..density.., 2)))) + 
      scale_fill_manual(values = RColorBrewer::brewer.pal(12,"Set3"), name = "Chain") +
      scale_color_manual(values = RColorBrewer::brewer.pal(12,"Set3"), name = "Chain") +
      geom_density(alpha=0.5 )+
      scale_x_continuous(trans = "log10", labels = label_comma()) +
      ggtitle(if_else(length(input$chains) != 1,"Selected chain(s): density dist. of LPs sizes",paste0(input$chains[[1]],": density dist. of LPs sizes"))) +
      xlab("LP size (USD)") + 
      ylab("Density")
    ggplotly(p0, tooltip = "text")
  })
  
  # Cumulative
  output$plot9 <-  renderPlotly({
    p1 <- filterChainsCompare() %>% 
      arrange(LP_POSITION_USD) %>%
      ggplot(., aes(x = LP_POSITION_USD, color = CHAIN,  text = paste0(..color..,"<br>",
                                                                                 "Position: ",scales::dollar(10^(..x..)),"<br>",
                                                                                 "Cumulative prob.: ",round(..y.., 4)))) +
      stat_ecdf(pad = FALSE) +
      scale_color_manual(values = RColorBrewer::brewer.pal(12,"Set3"), name = "Chain") +
      scale_x_continuous(trans = "log10", labels = label_comma()) +
      ggtitle(if_else(length(input$chains) != 1,"Selected pool(s): cumulative distribution LPs sizes",paste0(input$chains[[1]],": cumulative distribution LPs sizes"))) +
      xlab("LP size (USD)") + 
      ylab("Cumulative prob.")
    ggplotly(p1, tooltip = "text")
  })
}



shinyApp(ui = ui, server = server)