function(input, output) {
  
  output$plotly_1  <- renderPlotly({
    
    plot1 <- 
      epl_winner %>% filter(name %in% input$input_result) %>% 
      ggplot(mapping = aes(x=factor(Year), y=value,
                           text = label,
                           group=name))+
      geom_line(aes(color=name),position = "dodge")+
      geom_point(aes(color=name))+
      scale_y_continuous(breaks = seq(0,36,3))+
      labs(title = "Match Results Trend of The Champions",
           x="Year End of Season",
           y="",
           color="Results")+
      theme(text = element_text(size = 10),
            plot.title = element_text(hjust = 2),
            plot.subtitle = element_text(hjust = 1.5, size = 10))
    
    ggplotly(p = plot1, 
             tooltip = "text")
    
  })
  
  output$plotly_2 <- renderPlotly({
    
    team_pos <- epl_clean %>% 
      filter(Team%in% input$input_team) %>% 
      select(Season, Team, Pos) 
      
    team_pos <- team_pos %>% 
      mutate(label = glue("Position: {Pos}"))
    
    plot2 <- 
      team_pos %>% 
      ggplot(mapping = aes(x=Season, y=-Pos,
                           group=1,
                           text = label))+
      geom_line(col="red")+
      geom_point(col="red")+
      scale_y_continuous(limits = c(-20,-1) ,breaks = seq(-20,0,2))+
      labs(title = paste("Final Position of", input$input_team, "by Season"),
           x = "",
           y = "") + 
      theme_light() +
      theme(legend.position = "none")  +
      theme(text = element_text(size = 10),
            plot.title = element_text(hjust = 2),
            plot.subtitle = element_text(hjust = 1.5, size = 10))
    
    ggplotly(plot2, tooltip = "label")
    
  })
  
  output$plotly_3 <- renderPlotly({
    
    epl_pie <- epl_clean %>% 
      filter(Team %in% input$input_team & Season %in% input$input_season) %>%
      select(Team, Win, Draw, Lost) %>% 
      pivot_longer(c("Win", "Draw", "Lost")) %>%
      mutate(label = glue("{value}"))
    
    colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
    
    plot3 <- plot_ly(epl_pie, labels = ~name, values = ~value, type = 'pie',
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'),
                   hoverinfo = 'text',
                   text = ~paste(value, ' matchs'),
                   marker = list(colors = colors,
                                 line = list(color = '#FFFFFF', width = 1)),
                   showlegend = FALSE)
    plot3 <- plot3 %>% layout(title = glue("Match Result of {input$input_team}"),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    ggplotly(plot3)
  })
  
  output$plotly_4 <- renderPlotly({
    
    epl_goal <- epl_clean %>% 
      filter(Team %in% input$input_team & Season %in% input$input_season) %>%
      select(Team, Goal, Conceded) %>% 
      pivot_longer(c("Goal", "Conceded"))
    
    epl_goal <- epl_goal %>% 
      mutate(label=glue("Total {name} {value}"))
    
    plot4 <- epl_goal %>% 
      ggplot(mapping = aes(x=factor(name, levels=c("Goal", "Conceded")), 
                           y=value,
                           text=label))+
      geom_col(fill="purple")+
      scale_y_continuous(limits = c(0,110) ,breaks = seq(0,110,10))+
      labs(title = "Total Goal and Conceded",
           x="",
           y="")+
      theme(text = element_text(size = 10),
            plot.title = element_text(hjust = 2),
            plot.subtitle = element_text(hjust = 1.5, size = 10))
    
    ggplotly(plot4, tooltip = "label", height = 550)
  })
  
  output$plotly_5 <- renderPlot({
    
    epl_clean %>% 
      filter(Team %in% input$input_team & Season %in% input$input_season) %>%
      select(Team, Win, Draw, Lost) %>% 
      pivot_longer(c("Win", "Draw", "Lost")) %>%
      mutate(label = glue("{value}")) %>% 
      ggplot(mapping = aes(x=1, y=value, fill=name))+
      geom_col(position = "stack", show.legend = F)+
      geom_text(aes(label = paste(name, ': ', value)), 
                position = position_stack(vjust = .5))+
      coord_polar(theta = "y")+
      theme_void()
    
  })
  
  output$plotly_6 <- renderPlotly({
    
    plot6 <- epl_clean %>% ggplot(mapping = aes(x=Pos,y=Goal))+
      geom_jitter(col="red")+
      geom_smooth(method = "lm", se = FALSE)
    
    ggplotly(plot6)
    
  })
  
  output$plotly_7 <- renderPlotly({
    
    plot7 <- epl_clean %>% ggplot(mapping = aes(x=Pos,y=Conceded))+
      geom_jitter(col="red")+
      geom_smooth(method = "lm", se = FALSE)
    
    ggplotly(plot7)
    
  })
  
  output$plotly_8 <- renderPlotly({
    
    plot8 <- epl_clean %>% ggplot(mapping = aes(x=Goal,y=Conceded))+
      geom_jitter(col="red")+
      geom_smooth(method = "lm", se = FALSE)
    
    ggplotly(plot8)
    
  })
  
  output$most_goal <- renderValueBox({
    
    max_goal <- epl_clean %>% filter(Season %in% input$input_Season) %>% 
      select(Team, Goal) %>% 
      group_by(Team) %>% 
      summarise(goal=max(Goal)) %>% 
      ungroup() %>% 
      top_n(1)
    
    max_goal <- max_goal %>% 
      mutate(label=glue("{goal} by {Team}"))
    
    valueBox(max_goal$label, "Most Goals", icon = icon("circle-chevron-up"),
             color = "purple")
    
  })
  
  output$most_conceded <- renderValueBox({
    
    max_conceded <- epl_clean %>% filter(Season %in% input$input_Season) %>% 
      select(Team, Conceded) %>% 
      group_by(Team) %>% 
      summarise(conceded=max(Conceded)) %>% 
      ungroup() %>% 
      top_n(1)
    
    max_conceded <- max_conceded %>% 
      mutate(label=glue("{conceded} to {Team}"))
    
    valueBox(max_conceded$label, "Most Conceded", icon = icon("circle-chevron-down"),
             color = "purple")
    
  })
  
  output$total_goal <- renderValueBox({
    total_goal <- epl_clean %>%  
      group_by(Season) %>% 
      summarise(total_goal=sum(Goal)) %>% 
      ungroup() %>% 
      filter(Season %in% input$input_Season) %>% 
      select(total_goal)
    
    valueBox(total_goal, glue("Total Goals in {input$input_Season} Season"), icon = icon("futbol"),
             color = "red")
  })
  
  output$pos_1 <- renderValueBox({
    pos_1 <- epl_clean %>% 
      filter(Season %in% input$input_Season & Pos==1) %>% 
      select(Team)
    
    valueBox(pos_1, "First Position", icon = icon("trophy"),
             color = "purple")
  })
  
  output$pos_20 <- renderValueBox({
    pos_20 <- epl_clean %>% 
      filter(Season %in% input$input_Season & Pos==20) %>% 
      select(Team)
    
    valueBox(pos_20, "Last Position", icon = icon("arrow-trend-down"),
             color = "purple")
  })
  
  output$average_goal <- renderValueBox({
    average_goal <- epl_clean %>%  
      group_by(Season) %>% 
      summarise(average_goal=round(sum(Goal)/380,2)) %>% 
      ungroup() %>% 
      filter(Season %in% input$input_Season) %>% 
      select(average_goal)
    
    valueBox(average_goal, "Average Goals per Match", icon = icon("futbol"),
             color = "red")
  })
  
  output$df <- renderDataTable({
    
    datatable(epl_clean, options = list(scrollX = TRUE))
    
  })
  
}
