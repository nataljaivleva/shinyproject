
options(encoding = "utf-8")
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(bslib)
library(showtext)
library(thematic)
library(DT)
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(usmap)
library(rgdal)
library(ggplot2)
library(maps)
library(mapdata)
library(maptools)
library(plotly)
library(DataExplorer)
library(ggthemes)
#library(profvis)

df <- read.csv(file = 'DS_Jobs.csv')
df$job_state <- trimws(df$job_state)
df_us_cities <- read.csv(file = 'us_cities.csv')

data_type_character <- names(df)[which(sapply(df, is.character))]
data_type_numeric <- names(df)[which(sapply(df, is.numeric))]
data_type_factor <- names(df)[which(sapply(df, is.factor))]
size <- dim(df)

df_location <- df %>% count(df$Location, sort = TRUE)
names(df_location) <- c('location', 'count')
temp <- df %>% group_by(Location) %>% summarise(excel = sum(excel), 
                                                python = sum(python),
                                                hadoop = sum(hadoop), 
                                                spark = sum(spark),
                                                aws = sum(aws),
                                                tableau = sum(tableau),
                                                big_data = sum(big_data)
)
names(temp)[1] <- 'location'
df_location <- inner_join(df_location, temp, by = c('location'))
df_location <- df_location  %>% separate(location, sep = ",[^,]*$", into = c('city'), remove = FALSE)
df_location$state <- substr(df_location$location,start = nchar(df_location$location)-1, stop = nchar(df_location$location))
df_location <- df_location %>% drop_na()
df_location <- inner_join(df_location, df_us_cities, by = c('city','state'))
df_location <- df_location %>% filter(!duplicated(df_location$location))
df_location <-  df_location %>% select(long, lat, state, city, location, statename, count, python, excel, tableau, hadoop, spark,aws, big_data) 

df_transformed <- usmap_transform(df_location)

df_states <- df_location %>% filter(!duplicated(df_location$state,df_location$statename))
df_states <-  df_states %>% select(statename, state)
temp <- data.frame(cbind(statename = state.name,state = state.abb))
df_states <- union(temp,df_states)
df_states <- df_states  %>% arrange(statename)
choiceList <- df_states[, 2]
names(choiceList) <- paste0(df_states[, 1], " (", df_states[, 2],")")

df_programs <- df_location %>% group_by(state) %>% summarise(Excel = sum(excel), 
                                                             Python = sum(python),
                                                             Hadoop = sum(hadoop), 
                                                             Spark = sum(spark),
                                                             AWS = sum(aws),
                                                             Tableau = sum(tableau),
                                                             `Big Data` = sum(big_data),
                                                             count = sum(count),
                                                             statename = first(statename))

df_programs <- melt(setDT(df_programs), id.vars = c('state','statename','count'), variable.name = 'skills')


df_minmax <-  df %>% group_by(Location) %>% summarise(min_salary= min(min_salary), max_salary = max(max_salary),avg_salary = mean(avg_salary))
names(df_minmax)[1] <- 'location'
df_minmax <- df_minmax  %>% separate(location, sep = ",[^,]*$", into = c('city'), remove = FALSE)
df_minmax$state <- substr(df_minmax$location,start = nchar(df_minmax$location)-1, stop = nchar(df_minmax$location))
df_minmax <- df_minmax %>% drop_na()
df_minmax <- inner_join(df_minmax, df_us_cities, by = c('city','state'))
df_minmax <- df_minmax %>% select(state, statename, min_salary, max_salary, avg_salary)
df_minmax <- df_minmax %>% group_by(state, statename) %>% summarise(`Minimum`= min(min_salary), `Maximum` = max(max_salary),`Average` = round(mean(avg_salary)))
df_minmax <- melt(setDT(df_minmax), id.vars = c('state','statename'), variable.name = 'salary')


my_theme <- bs_theme(bootswatch = "superhero") #cerulean
#                     ,base_font = font_google("Righteous"))
#thematic_shiny(font = "auto")



ui <- fluidPage(  
    theme = my_theme,
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
        mainPanel(width = "auto",
            HTML(r"(<div class ="container-fluid">)"),
            #menu
            HTML(r"(
                <nav class="navbar navbar-expand-lg navbar-light bg-light fixed-top">
                <a class="navbar-brand" href="#">Project</a>
                <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarNav" aria-controls="navbarNav" aria-expanded="false" aria-label="Toggle navigation">
                <span class="navbar-toggler-icon"></span>
                </button>
                <div class="collapse navbar-collapse" id="navbarNav">
                <ul class="navbar-nav">
                <li class="nav-item active">
                <a class="nav-link" href="#home">Home<span class="sr-only">(current)</span></a>
                </li>
                <li class="nav-item">
                <a class="nav-link" href="#dataset">Dataset</a>
                </li>
                <li class="nav-item">
                <a class="nav-link" href="#visual">Visualization</a>
                </li>
                </ul>
                </div>
                </nav>
            )"),
            # description
            HTML(r"(<br>
                 <h1 id="home" style="padding-top:50px">Data Science Job Posting on Glassdoor</h1>
                 <p>Subject: Data Visualization (ITB8812)<br>
                 Teacher: Olga Dunajeva<br>
                 Student: Natalja Ivleva</p>
                 <br>
                 <h2>Description</h2>
                 <p>The world is advancing with a growing generation of data. However, this advancement is dependent on how efficiently data is being used. 
                 All the data generated by an organization works as a great asset in progressing its work culture and productivity. 
                 Making good use of data helps companies make better and informed decisions and form a significant strategy to prosper their growth.
                 And to make these organizations achieve this, Data Science and Data Scientists come into play (<a href='https://www.analyticsinsight.net/increasing-popularity-data-scientists-may-cause-supply-talent/'>Source</a>).</p>
                 <p>Today are very popular professions that are related to data processing (Data Science, Big Data, Machine Learning ...). 
                 Many companies are searching specialists in this area.</p>
                 <p>Millions of people are searching for jobs, salary information, company reviews, and interview questions. 
                 See what others are looking for on <a href='https://www.glassdoor.com/index.htm' target="_blank">Glassdoor</a> today.</p><br>
                 <br><ul><li>This application uses dataset of data science job posts in the USA</li>
                 <li>The DataSet section describes the structure.</li>
                 <li>The Visualization section consists of: </li><ul><li>Maps of the distribution of vacancies by state and city</li>
                 <li>Stacked bar chart - what knowledge and skills you need to have to get a job</li>
                 <li>Chart Lines  shows the minimum, maximum and average salaries by state</li></ul></ul><br>
                                  )"),
            
            HTML(r"(
                 <h2 id="dataset" style="padding-top:50px">DataSet</h2>
                 <div class="row">
                 <div class="col col-lg-4">
                 <p>Source: <a href="https://www.kaggle.com/rashikrahmanpritom/data-science-job-posting-on-glassdoor" target="_blank">Kaggle</a></p>
                 <p>This is a dataset of data science job posts in glassdoor.</p>
                 <h5>Content</h5>
                 <p>The data was scrapped from glassdoor's website. 
                 There are two versions of the data one is uncleaned and another one is cleaned. 
                 Web scrapping, Data cleaning & EDA code are added in the code section.</p>
                 
                 <p>In the cleaned version the columns explanation are as follows,</p>
                 <ul>
                 <li><b>Job Title</b>: Title of the job posting</li>
                 <li><b>Salary Estimation</b>: Salary range for that particular job</li>
                 <li><b>Job Description</b>: This contains the full description of that job</li>
                 <li><b>Rating</b>: Rating of that post</li>
                 <li><b>Company</b>: Name of company</li>
                 <li><b>Location</b> Location of the company</li>
                 <li><b>Headquarter</b>: Location of the headquater</li>
                 <li><b>Size</b>: Total employee in that company</li>
                 <li><b>Type of ownership</b>: Describes the company type i.e non-profit/public/private farm etc</li>
                 <li><b>Industry, Sector</b>: Field applicant will work in</li>
                 <li><b>Revenue</b>: Total revenue of the company</li>
                 <li><b>minsalary,maxsalary,avgsalary</b>: Refers to the minimum, maximum and average salary</li>
                 <li><b>jobstate</b>: State where the applicant will work</li>
                 <li><b>samestate</b>: Same state as headquarter or not(Boolean)</li>
                 <li><b>companyage</b>: Age of company</li>
                 <li><b>python,excel,hadoop,spark,aws,tableau,bigdata</b>: Some most appeared skills in boolean columns </li>
                 <li><b>jobsimp</b>: Job type</li>
                 <li><b>seniority</b>: if job type is senior or not (Boolean)</li>
                 </ul>
                 </div>
                 <div class="col col-lg-8">)"),
                 DT::DTOutput(outputId = "table"),
            HTML(r"(</div>
                 </div>
                <hr>
                <div>)"),
                h5("Dataset contains",size[1],"rows and ",size[2], "columns"),
          
                tags$table(class = "table table-striped",
                       tags$tr(
                           tags$th("Type"),
                           tags$th("Count"),
                           tags$th("Columns")
                       ),
                       tags$tr(
                           tags$td("Character"),
                           tags$td(length(data_type_character)),
                           tags$td(paste(data_type_character,collapse="; "))
                       ),
                       tags$tr(
                           tags$td("Numeric"),
                           tags$td(length(data_type_numeric)),
                           tags$td(paste(data_type_numeric,collapse="; "))
                       ),
                       tags$tr(
                           tags$td("Factor"),
                           tags$td(length(data_type_factor)),
                           tags$td(paste(data_type_factor,collapse="; "))
                       )
                 ),
            #HTML(r"(<br><p>Example: using the library DataExplorer 
                # (<a href="https://cran.r-project.org/web/packages/DataExplorer/vignettes/dataexplorer-intro.html" target = "_blank">Documentation</a>).</p><br>)"),
            
            #plotOutput("df_hist"),
            #HTML(r"(<br><hr style="width: 100%; color: white; height: 1px; background-color:white;"><br>)"),
            #plotOutput("df_corr"),
            
            HTML(r"( </div>
            )"),
           
            HTML(r"(</div>)"),

            HTML(r"(<h2 id="visual" style="padding-top:50px">Visualization</h2>)"),
            dashboardPage(
                dashboardHeader(title = "Data Science Job Posting on Glassdoor",disable = T),
                dashboardSidebar(
                    useShinyjs(),
                    tags$style(HTML(r"(.main-sidebar{padding-top:0;}")")),
                    pickerInput("selectState","Select state:", choices=choiceList, selected = c(df_states$state),
                                options = list(`actions-box` = TRUE), multiple = T),
                    HTML(r"(<h5 id='top'>TOP (N = Vacancies)</h5>)"),
                    tableOutput('tableLocation'),
                    HTML(r"(<br>)"),
                    tableOutput('tableCompany'),
                    HTML(r"(<br>)"),
                    tableOutput('tableJobTitle')
                    
                ),
                dashboardBody(
                    verbatimTextOutput("textSelected"),
                   
                    fluidRow(responsive=TRUE,
                        box(title = "Data Science Job in Each US State", status = "warning", solidHeader = TRUE,
                            plotlyOutput("mapUSA", height = "300px")),
                        
                        box(title = "Skills", status = "warning", solidHeader = TRUE,
                            plotlyOutput("mapPrograms", height = "300px")),
                    ),
                    fluidRow(
                        box(title = "Data Science Job in Selected US State", status = "warning", solidHeader = TRUE,
                            plotlyOutput("mapStates", height = "300px")),
                        box(title = "Salary ...  thousands $ per year", status = "warning", solidHeader = TRUE,
                            plotlyOutput("mapSalary", height = "300px"))
                        
                    )
                ),
               tags$head(
                    tags$style(HTML(".skin-blue .main-sidebar {background-color:#2b3e50;}"))
                )
                
            )
            
           
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    theme = my_theme
    output$table <- DT::renderDT(df, extensions=c("Responsive", "Buttons"), 
                                 class = "display",
                                 options = list(paging = TRUE, pageLength = 10,
                                                dom = 'Bfrtip',
                                                buttons = c('colvis','csv','excel','pdf')
                                                #,columnDefs = list(list(visible=FALSE, targets=c(3)))
                                                ))
    observe({
        output$mapUSA <- renderPlotly({
            g <- plot_usmap("states", labels = TRUE, label_color = "gray",
                            color = "orange", size = 0.5, fill = "yellow", alpha = 0.25) +
                scale_size_continuous(range = c(1, 10),
                                      label = scales::comma)+                 
                geom_point(data = df_transformed, inherit.aes=F,
                           aes(x = long.1, y = lat.1, size = count, text = paste("State:",statename,"\nCity:", city, "\nCount:", count)), 
                           color = "green", alpha = 0.3) 
             
            ggplotly(g, tooltip = c("text"))
            
        })
        output$mapStates <- renderPlotly({
            df_transformed_states <- df_transformed[which(df_transformed$state %in% input$selectState),]
            g <- plot_usmap("states", include = input$selectState, labels = T, label_color = "gray",
                            color = "orange", size = 0.5, fill = "yellow", alpha = 0.25) +
                scale_size_continuous(range = c(1, 10),
                                      label = scales::comma)
            
            if (nrow(df_transformed_states)>0){
                g <- g +                 
                    geom_point(data = df_transformed_states, inherit.aes=F,
                               aes(x = long.1, y = lat.1, size = count, 
                                   text = paste("State:",statename,"\nCity:", city, "\nCount:", count)), 
                               color = "green", alpha = 0.3)
                ggplotly(g, tooltip = c("text"))
            } 
           
            
        })
        
        output$mapPrograms <- renderPlotly({
            df_programs_sel <- df_programs[which(df_programs$state %in% input$selectState),]
            if(nrow(df_programs_sel)>0){
            g <- ggplot(df_programs_sel,aes(fill=skills, y=value, x=paste0(state,'(',count,')'),
                                         text = paste("State:",statename,"\nCount:",value, "\nSkills:", skills,"\nPercent of vacancies:", round(value*100/count,1),"%"))) + 
                geom_bar(position="fill", stat="identity", width = 0.5) + 
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(0.8)), 
                      axis.title.x = element_text(size=rel(0.7))) +
                theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +  
                theme(legend.title = element_blank(), legend.text = element_text(size = 7)) + 
                labs(x ="State(count vacancies)") + ylab("")
            
                ggplotly(g, tooltip = c('text')) %>%
                    layout(legend = list(orientation = "h", title="",  x = 0, y = 1.3,
                                         justification=c(0.5, 1)))
                
            }  
            
        })
        
        output$mapSalary <- renderPlotly({
            df_minmax_sel <- df_minmax[which(df_minmax$state %in% input$selectState),]
            if(nrow(df_minmax_sel)>0){
            g <- ggplot(df_minmax_sel, aes(x=state,y=value, colour=salary, group = salary, 
                                       text = paste("State:",statename,"\nType:",salary, "\n",value," thousands $ per year"))) + geom_point() +  geom_line() +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=rel(0.8)),
                      axis.title.x = element_text(size=rel(0.7))) +
                theme(axis.text.y = element_text(size=rel(0.8))) +
                theme(legend.title = element_blank(), legend.text = element_text(size = 7)) + 
                ylab("")+ labs(x ="State")
            ggplotly(g, tooltip = c('text'))%>%
                layout(legend = list(orientation = "h", y = 1.2, title="", justification=c(0.5, 1)))
            }
        })
        
        temp <- df[which(df$job_state %in% input$selectState),]
        
        if(nrow(temp)>0 ){
            top5Location <- temp %>% group_by(Location) %>% summarize(count=n()) %>%  arrange(desc(count)) %>% head(5)
            top5Company <-  temp %>% group_by(Company.Name) %>% summarize(count=n()) %>%  arrange(desc(count)) %>% head(5)
            top5JobTitle <- temp %>% group_by(Job.Title) %>% summarize(count=n()) %>%  arrange(desc(count)) %>% head(5)
            
            names(top5Location)[1] <- 'Location'
            names(top5Location)[2] <- 'N'
            output$tableLocation <- renderTable(top5Location, width = "200px", striped = TRUE,
                                                options = list(
                                                    columnDefs = list(list(width = '50px', targets = c(1)))))
            
            names(top5Company)[1] <- 'Company'
            names(top5Company)[2] <- 'N'
            output$tableCompany <- renderTable(top5Company, width = "200px", striped = TRUE,
                                               options = list(
                                                   columnDefs = list(list(width = '50px', targets = c(1)))))
            
            names(top5JobTitle)[1] <- 'Job Title'
            names(top5JobTitle)[2] <- 'N'
            output$tableJobTitle <- renderTable(top5JobTitle,width = "200px", 
                                                options = list(
                                                    columnDefs = list(list(width = '50px', targets = c(1)))))
        } 
        if(is.null(input$selectState))
        {
            output$textSelected <- renderText("Not selected state")
            shinyjs::hide(id = "top")
            hideElement("tableLocation")
            shinyjs::hide(id = "tableCompany")
            shinyjs::hide(id = "tableJobTitle")
        } else{
            if (length(df_states$state)==length(input$selectState))
                output$textSelected <- renderText("You selected all states")
            else
                output$textSelected <- renderText({paste("States: ",paste(input$selectState, collapse = ", "))})
            shinyjs::show(id = "top")
            shinyjs::show(id = "tableLocation")
            shinyjs::show(id = "tableCompany")
            shinyjs::show(id = "tableJobTitle")
        }

                               

    })
    
    output$df_hist <- renderPlot({
       plot_histogram(df, title = "Distributions for all continuous features")

    })
    output$df_corr <- renderPlot({
        plot_correlation(na.omit(df), title = "Correlation Analysis with continuous features", maxcat = 5L, type = "c")
    })
    


    
}

# Run the application 
shinyApp(ui = ui, server = server)

