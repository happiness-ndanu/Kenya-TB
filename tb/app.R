library(shiny)
library(tidyverse)
# library(summarytools)
# library(glue)
# library(patchwork)
library(readxl)
library(here)
library(janitor)
# library(graphics)
library(paletteer)
# library(lubridate)
library(sf)
# library(ggplot2)
#library(tmap)
library(leaflet)
# library(dplyr)
# library(wesanderson)
# names(wes_palettes)


#Read Data
#here::i_am("TB_Kenya/app.R")
# Read and save as RDS files so that we can minimise the app size!!!
#cleannames is used to put all titles into small letters and add underscores to words instead of spacing them

# tuberclosis <-read_excel("tuberclosis.xlsx") %>%  
#   clean_names()
#saveRDS(tuberclosis, "tuberclosis")

# Read RDS 
tuberclosis <- readRDS("tuberclosis")


#Detect Missing Values
anyNA(tuberclosis)

#Sum of Missing Values
colSums(is.na(tuberclosis))

#Replace missing values in each column
#Replacing missing values with "not done" answer in the cotrimoxazole...column
tuberclosis <- tuberclosis %>% 
  mutate(cotrimoxazole_preventive_therapy_y_n=replace_na(cotrimoxazole_preventive_therapy_y_n,"not_done"))%>% 
  mutate(cotrimoxazole_preventive_therapy_y_n=case_when(
    cotrimoxazole_preventive_therapy_y_n=="N/A"~"not_administered",TRUE~cotrimoxazole_preventive_therapy_y_n)) %>% 
  mutate(zone=replace_na(zone, "madaraka_zone_b")) %>% 
  mutate(health_facility=replace_na(health_facility, "metropolitan_hospital_nairobi")) %>% 
  mutate(regimen=replace_na(regimen,"not_administered")) %>% 
  mutate(art_y_n=replace_na(art_y_n,"not_administered")) %>% 
  mutate(art_y_n=case_when(
    art_y_n=="N/A"~"not_applicable",
    TRUE~art_y_n
  )) %>% 
  mutate(art_y_n=case_when(
    art_y_n=="Y"~"administered",
    TRUE~art_y_n
  )) %>% 
  mutate(comorbidity=replace_na(comorbidity,"not_diagnosed"))%>% 
  mutate(xray=replace_na(xray, "not_done")) %>% 
  mutate(xray=case_when(
    xray=="ND"~"not_determined",
    TRUE~xray
  ))

#selecting desired columns
newtuberclosis <- tuberclosis %>% 
  select(serial_number:is_patient_prisoner, sex_m_f:height_mtrs, dot_by,type_of_tb_p_ep, type_of_patient, xray, sputum_smear_examination_0th_month_result, sputum_smear_examination_2by3_month_result,sputam_smear_examination_5th_month_result, sputum_smear_examination_6by8_month_result, regimen,hiv_status,partner_hiv_status, cotrimoxazole_preventive_therapy_y_n, art_y_n,nutrition_support, comorbidity, treatment_outcome,is_from_tibu_lite)

#check if missing values are present
colSums(is.na(newtuberclosis)) 




#Calculate The Age Distribution of Patients
# 
# agetb<- newtuberclosis %>%
#   select(age_on_registration)
# show_distribution(var_data = agetb, binwidth = 10)




#read in the kenyan shapefiles
# 
# countySHP <- read_sf("D:\\goodshapefile\\currentshapefiles\\Counties.shp", quiet = TRUE, stringsAsFactors = FALSE,as_tibble = TRUE)
# saveRDS(countySHP, "countySHP.rds")
#readRDS(file = "countySHP.rds")
countySHP <- readRDS("countySHP.rds")

#View(countySHP %>% st_drop_geometry())

#inspect a few rows
print(countySHP[6:9], n = 3)

#inspect column names
colnames(countySHP)

#inspect the class shape files
class(countySHP)

#Look at the variable data types
glimpse(countySHP)

#View the geometry column
countySHP_geometry <- st_geometry(countySHP)
# View one geometry entry
countySHP_geometry[[1]]

#Geometry columns have their own class
class(countySHP) #sfc, the list-column with the geometries for each feature
#> [1] "sfc_MULTIPOLYGON" "sfc"

class(countySHP[[1]]) #sfg, the feature geometry of an individual simple feature
#> [1] "XY"           "MULTIPOLYGON" "sfg"

### This line is not necessary since the shapefile is already in the WGS 84 projection.

countySHP <- st_transform(countySHP, crs = 4326)

### Inspect the co-ordinate reference system
st_crs(countySHP)
# Coordinate Reference System:
#   User input: EPSG:4326 
# wkt:
#   GEOGCRS["WGS 84",
#           ENSEMBLE["World Geodetic System 1984 ensemble",
#                    MEMBER["World Geodetic System 1984 (Transit)"],
#                    MEMBER["World Geodetic System 1984 (G730)"],
#                    MEMBER["World Geodetic System 1984 (G873)"],
#                    MEMBER["World Geodetic System 1984 (G1150)"],
#                    MEMBER["World Geodetic System 1984 (G1674)"],
#                    MEMBER["World Geodetic System 1984 (G1762)"],
#                    MEMBER["World Geodetic System 1984 (G2139)"],
#                    ELLIPSOID["WGS 84",6378137,298.257223563,
#                              LENGTHUNIT["metre",1]],
#                    ENSEMBLEACCURACY[2.0]],
#           PRIMEM["Greenwich",0,
#                  ANGLEUNIT["degree",0.0174532925199433]],
#           CS[ellipsoidal,2],
#           AXIS["geodetic latitude (Lat)",north,
#                ORDER[1],
#                ANGLEUNIT["degree",0.0174532925199433]],
#           AXIS["geodetic longitude (Lon)",east,
#                ORDER[2],
#                ANGLEUNIT["degree",0.0174532925199433]],
#           USAGE[
#             SCOPE["Horizontal component of 3D system."],
#             AREA["World."],
#             BBOX[-90,-180,90,180]],
#           ID["EPSG",4326]]

# #Load the data that we are going to map
#View(newMH)

#Clean the data, so that the sub-counties match those in the shapefile.

### Inspect the county names of the TB data
counties_df <- unique(newtuberclosis$county)

### Inspect the county names of the shape file
counties_KenyaSHP <- countySHP %>% 
  st_drop_geometry() %>% 
  select(NAME_1) %>% 
  pull() %>%
  unique()
glimpse(counties_KenyaSHP)

### Convert the TB county names to title case
tb_df <- newtuberclosis %>% 
  ungroup() %>% 
  mutate(county = tools::toTitleCase(tolower(county)))

### Inspect the county names of the TB data again 
counties_tb_df <- unique(tb_df$county)

### Inspect the county names that are different in each of the datasets
unique(tb_df$county)[which(!unique(tb_df$county) %in%counties_KenyaSHP)]


### Clean the county names so that they match in both datasets

tb_df <- tb_df %>% 
  mutate(county= ifelse(county == "Elgeyo Marakwet", "Elgeyo-Marakwet",
                        ifelse(county =="Pokot","West Pokot",
                               ifelse(county == "Tharaka Nithi", "Tharaka-Nithi", county))))


### Inspect the county names again to ensure that they now match.
unique(tb_df$county)[which(!unique(tb_df$county) %in% counties_KenyaSHP)]

#make totals column
tb_dfcount <-tb_df %>% 
  count(county, sort = TRUE)


#Join the shapefile and the data
### Rename the COUNTY variable, to match the variable name in the shapefile data
colnames(tb_dfcount)[1]="NAME_1"
#the above column has been renamed the same as the subcountiesSHP dataframe column that has names of sub counties
colnames(tb_dfcount)[2]="tb_totals"
#View(tb_dfcount)

### Ensure that there are no leading or trailing spaces in the county variable
countySHP$NAME_1 <- trimws(countySHP$NAME_1)
tb_dfcount$NAME_1 <- trimws(tb_dfcount$NAME_1)

### Merge the data
merged_tb_df <- left_join(tb_dfcount, countySHP,by = "NAME_1")
# View(merged_tb_df)

### Sort the data so that the County variable appears first
merged_tb_df <- merged_tb_df %>% 
  select(NAME_1, everything())



### Class of the merged data
class(merged_tb_df)
### Column names
colnames(merged_tb_df)



#Visualise the data
#plot()
#We are going to plot a base plot / map.

plot(countySHP$geometry, lty = 3, col = "chocolate")

#ggplot2()
map1 <- ggplot(data = merged_tb_df)+
  geom_sf(aes(geometry = geometry, fill = tb_totals))+
  theme_void()+
  labs(title = "Distribution of Tuberclosis Cases In Different Counties in Kenya",
       caption = "By:Happiness Ndanu")+
  theme(plot.title = element_text(family = "URW Palladio L, Italic",size = 16, hjust = 0.5),
        legend.title = element_blank(),
        plot.caption = element_text(family = "URW Palladio L, Italic",size = 12))+
  #scale_fill_gradient(low = "darkgreen", high = "red")
  scale_fill_viridis_c(option = "A")
map1


#9.3 tmap()
tmap_mode("plot") #Set tmap mode to static plotting or interactive viewing
merged_tb_df <- st_sf(merged_tb_df)

map2 <- tm_shape(merged_tb_df) +
  tm_fill("tb_totals",palette="YlOrRd",
          title="Distribution of TB Cases In Kenyan Counties",
          id = "NAME_1") +
  tm_borders(col = "blue",lty = 3)+
  tm_layout(legend.position = c("left", "bottom"))+
  tmap_mode(mode = "view")
map2



#9.4 leaflet()

## Specify the color scheme
pal <-colorBin(
  palette = "Dark2",
  domain = merged_tb_df$tb_totals)

### Specify how labels will be displayed
labels <- sprintf(
  "<strong>%s</strong><br/>%g",
  merged_tb_df$NAME_1, merged_tb_df$tb_totals
) %>% lapply(htmltools::HTML)


##Generate the graph
leaflet(merged_tb_df) %>%
  addTiles() %>%
  addPolygons(color = "blue", weight = 1, dashArray = "3", fillColor = ~pal(tb_totals),
              highlight= highlightOptions(
                weight = 4,
                color = "blue",
                dashArray = "",
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight"="normal", padding="3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(position = c("bottomright"), pal,values = ~tb_totals)


counties= newtuberclosis %>% 
  distinct(county) %>% 
  pull(county)

sector= newtuberclosis %>% 
  distinct(sector) %>% 
  pull(sector)


PlotTB <- newtuberclosis %>%
  count(county,sector)







# Define UI --------------------------------------------------------------------

ui <- navbarPage(
  
  title = HTML(paste("TUBERCULOSIS EDA - KENYA")),
  
  tabPanel(
    title = "OVERVIEW",
    br(), br(),
    tags$img(height = 900, width = 1000,src = "teebee.jpg"),
    br(), br(),
    HTML(paste("In Kenya, Tuberculosis is ranked as the 5th leading cause of death. The spread of the disease is mostly as a result of breathing air that contains air droplets from an infected person(through sneezing and coughing). Most cases recorded are from busy and congested counties, such as Nairobi, which continuously pose as a threat since it hard to control. Luckily, the government of Kenya has implemented free TB diagnosis and treatment in public hospitals and Faith Based Organizations. In addition to this, sensitization on preventive measures and care of TB patients has played a huge role in reducing the number of cases. This dashboard contains a detailed EDA based on data collected across different counties.")),
    br(),
    HTML(paste("The aim of this analysis is to have a detailed Exploratory Data Analysis on the rampant cases recorded in Kenya.")),
    
    br(),br(),
    
    
    
    
    br(), br(),),
  
  tabPanel(
    title = "PLOTS",
    
    sidebarLayout(
      
      sidebarPanel(
        
        selectInput(
          inputId = "select_county",
          label = "Select County",
          choices = counties,
          multiple = FALSE,
          selected = "Samburu"
        ),
        
        selectInput(
          inputId = "select_sector",
          label = "Select Hospital Category",
          choices = sector,
          multiple = TRUE,
          selected = "Private"),
        HTML(paste("<b>","PLOTS SUMMARY","</b>")),br(),
        HTML(paste("In this survey, the age of majority of TB patients was 31 years.It seems that cumulatively, people above the age of 36 years (adults) recorded the highest number of TB patients whereas teenagers (between 13-19 years) had the least record of TB patients.")),
        br(),
        HTML(paste("Nairobi had the highest number of TB cases. This can be linked to the fact that it is a busy and congested town. It is also well known for having many industries and slums.")),
        br(),
        HTML(paste("Pulmonary TB is the most prevalent type of Tuberculosis recorded. It is a highly transmittable disease that can be transmitted through breathing air that has droplet of the cough or sneeze of an infected person.")),
        br(),
        HTML(paste("Based on the analysis, the margin between HIV negative and positive TB patients was quite major with close to 51%. HIV is known to weaken the immunity hence increasing risk of TB.")),
        br(),
        HTML(paste("Most counties recorded the highest percentage of ARV administration to HIV+ TB patients with counties such as Homabay having 100% success rate. This is quite impressive despite the high stigma surrounding HIV.")),
        br(),br(),
        HTML(paste("Below are explanations of the different initials for the treatment outcomes of TB patients:")),
        br(),
        HTML(paste("~ TC-treatment completed (A TB patient who completed treatment without evidence of failure).")),
        br(),
        HTML(paste("~ C- cured.")),
        br(),
        HTML(paste("~ D- died(is defined as TB patient who dies for any reason before starting or during the course of treatment),")),
        br(),
        HTML(paste("~ LTFU-Lost to follow up—A TB patient who did not start treatment or whose treatment was interrupted for 2 consecutive months or more.")),
        br(),
        HTML(paste("~ NC- not communicated.")),
        br(),
        HTML(paste("~ TO- transferred out to another unit.")),
        br(),
        HTML(paste("~ MT4- people who require 4th category treatment due to drug resistance.")),
        br(),
        HTML(paste("~ F- treatment failure (is a TB patient whose sputum smear or culture is positive at month 5 or later during treatment.)")),
        br(),
        ),
     
      mainPanel(
        plotOutput(outputId = "countysector"),
        plotOutput(outputId = "agecategory"),
        plotOutput(outputId = "countycases"),
        plotOutput(outputId = "tbtype"),
        plotOutput(outputId = "hiv"),
        plotOutput(outputId = "hivpos"),
        plotOutput(outputId = "arv"),
        plotOutput(outputId = "treatment"),
        plotOutput(outputId = "success"),
        plotOutput(outputId = "hostreatment"))
      
    )),
  
  
  navbarMenu("More",
             
             tabPanel(
               title = "Interactive Map",
               HTML(paste("<b>","VISUAL REPRESENTATION OF TUBERCULOSIS CASES ACROSS DIFFERENT COUNTIES", "</b>")),
               br(), br(),
               leafletOutput("mymap")
               
             ),
                 
                 tabPanel(
                   title = "Age Factor",
                   sidebarLayout(
                     
                     sidebarPanel(
                       sliderInput(inputId = "age_factor",
                                   label = h3("Select Age"),
                                   min = 1,
                                   max = 100,
                                   value = c(1, 10)),
                       
                       HTML(paste("<b>","Select age range and see the projections of count in different Sub-Counties and Health Facilities across Kenya", "</b>")),
                       br(), br(),
                      
                       sliderInput(inputId = "zoneage_factor",
                                   label = (""),
                                   min = 1,
                                   max = 1,
                                   value = c(1, 10))
                       
                     ),
                 
                 # Main panel for outputs
                 mainPanel(
                  plotOutput(outputId = "agefactor", width = "auto", height = "500px"),
                  plotOutput(outputId = "zoneagefactor", width = "auto", height = "500px"))
               ))
  ))

# Define server ----------------------------------------------------------------
server <- function(input, output, session) {
  
  output$countysector<- renderPlot({
    
    PlotTB %>% 
      filter(county %in% input$select_county) %>%
      filter(sector %in% input$select_sector) %>%
      mutate(sector= fct_reorder(sector, n)) %>%
      ggplot(aes(x= sector, y = n)) +
      geom_col(aes(fill = sector),
               position = "dodge",
               show.legend = TRUE,
               alpha = 0.5)+
      theme_minimal() +
      paletteer::scale_fill_paletteer_d("ggthemes::Classic_Green_Orange_6")+
      theme(
        axis.text.x = element_text(angle = 90)
      )+
      labs(
        x= 'SECTOR',
        y= 'NUMBER OF CASES PER COUNTY',
        title = 'TUBERCULOSIS CASES PER COUNTY AGAINIST HOSPITAL SECTOR CATEGORY'
      )+
      geom_text(aes(label = n, y =n))+
      theme(panel.grid = element_blank())
    
  })
  
  output$agecategory <- renderPlot({
    #converting the age values into categories
    newtuberclosis %>% 
      mutate(age_on_registration=as.numeric(as.character(age_on_registration))) %>% 
      mutate(ageascategorical= case_when(
        age_on_registration <= 12~"child",
        age_on_registration >=13 & age_on_registration<=19~"teenager",
        age_on_registration >=20 & age_on_registration<=35~"youth",
        age_on_registration >=36~"adult"
      )) %>% 
      ggplot(aes(x=ageascategorical))+
      geom_bar(mapping = aes(fill=ageascategorical),alpha= 0.7,data = NULL,stat = "count",position = "stack",na.rm = FALSE,orientation = NA, show.legend = NA,inherit.aes = TRUE)+
      
      labs(title = "COUNT OF DIFFERENT AGE GROUPS OF TB PATIENTS", x="AGE GROUPS", y="COUNT")+ 
      theme(plot.background = element_rect(fill = "white"))+
      paletteer::scale_fill_paletteer_d("ggsci::default_nejm")
    
  })

  
  output$countycases <- renderPlot({
    newtuberclosis %>% 
      count(county, sort = TRUE) %>% 
      #make county a factor
      mutate(county=factor(county)) %>% 
      #rearrange in descending order
      mutate(county= fct_reorder(county, n, .desc = TRUE)) %>% 
      slice_head(n=10) %>% 
      ggplot(mapping = aes(x=n, y= county))+
      geom_col(
        aes(fill = county), show.legend = TRUE) +
      ggtitle("COUNT OF TB PATIENTS IN THE FIRST 10 COUNTIES")+
      xlab("COUNTIES")+
      ylab("COUNT")+
      geom_text(aes(label=n),
                hjust=1,nudge_x = -.5,
                size = 3, fontface = "bold", family = "Fira Sans")+
      paletteer::scale_fill_paletteer_d("rcartocolor::Pastel")+
      #scale_fill_manual(values = pal, guide = "none") +
      theme_minimal()
    
  })
  
  output$tbtype <- renderPlot({
    newtuberclosis %>% 
      count(county, type_of_tb_p_ep, sort = TRUE) %>% 
      #make county a factor
      mutate(county=factor(county)) %>% 
      #rearrange in descending order
      mutate(county= fct_reorder(county, n, .desc = TRUE)) %>% 
      #grouping by the counties 
      #NB: remember to ungroup after using group_by()
      group_by(county) %>% 
      #Add a new column that does the totals of the type of tb
      mutate(totals=sum(n),
             percent= round(n/totals*100)) %>% 
      ungroup() %>% 
      slice_max(order_by = totals, n=20) %>% 
      ggplot(mapping = aes(x= percent, y= county))+
      geom_col(
        aes(fill = type_of_tb_p_ep),position ="dodge",show.legend = TRUE) +
      ggtitle("COUNT OF TYPE OF TB IN THE FIRST TOP 10 COUNTIES")+
    xlab("PERCENTAGE COUNT")+
      ylab("COUNTIES")+
      geom_text(aes(label=percent))+
      theme( 
        #rotate the x and y axis to make them eligible
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(angle = 0))
    
  })
  
  output$hiv <- renderPlot({
    newtuberclosis %>% 
      count(hiv_status, sort = TRUE) %>% 
      #make hiv status a factor
      mutate(hiv_status=factor(hiv_status)) %>% 
      #rearrange in descending order
      mutate(hiv_status= fct_reorder(hiv_status, n, .desc = TRUE)) %>% 
      ggplot(mapping = aes(x=hiv_status, y= log10(n)))+
      geom_col(
        aes(fill = hiv_status), show.legend = FALSE) +
      ggtitle("HIV STATUS OF TB PATIENTS")+
      xlab("HIV STATUS")+
      ylab("LOg10(n)")+
      geom_text(aes(label = n, y =log10(n)+0.3))+
      paletteer::scale_fill_paletteer_d("rcartocolor::Pastel")+
      theme(
        plot.title = element_text(hjust = 0.5))
  })

  output$hivpos <- renderPlot({
    newtuberclosis %>% 
      #select desired columns
      select(hiv_status,partner_hiv_status)%>%
      #filter only hiv positive patients
      filter(hiv_status=="Pos")%>%
      #count the outcome of the partners of hiv positive tb patients
      count(partner_hiv_status, sort = TRUE) %>% 
      #make partner_hiv_status column a factor
      mutate(partner_hiv_status=factor(partner_hiv_status)) %>% 
      #rearrange in descending order
      mutate(partner_hiv_status= fct_reorder(partner_hiv_status, n, .desc = TRUE))%>% 
      ggplot(mapping = aes(x= partner_hiv_status, y= n))+
      geom_col(
        aes(fill = partner_hiv_status), show.legend = FALSE) +
      ggtitle("COUNT OF PARTNERS' HIV STATUS(FOR HIV+ PATIENTS))")+
      xlab("PARTNERS HIV OUTCOME OF HIV+ TB PATIENTS")+
      ylab("PARTNERS OUTCOMES COUNT")+
      geom_text(aes(label = n, y =n+1.0))+
      paletteer::scale_fill_paletteer_d("rcartocolor::Pastel")+
      theme(
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  output$arv <- renderPlot({
    newtuberclosis %>% 
      select(hiv_status, art_y_n, county) %>% 
      filter(hiv_status=="Pos") %>% 
      count(county, art_y_n, sort = TRUE) %>% 
      #make county a factor
      mutate(county=factor(county)) %>% 
      #rearrange in descending order
      mutate(county= fct_reorder(county, n, .desc = TRUE)) %>% 
      group_by(county) %>% 
      #Add a new column that does the totals of the type of tb
      mutate(totals=sum(n),
             percent= round(n/totals*100)) %>% 
      ungroup() %>% 
      slice_max(order_by = totals, n=20) %>% 
      ggplot(mapping = aes(x=percent, y= county),)+
      geom_col(aes(fill=art_y_n),position = "dodge", show.legend = TRUE)+
      #add title  
      ggtitle("VISUAL REP. OF ARV ADMINISTRATION IN COUNTIES WITH HIGHEST HIV CASES")+
      xlab("PERCENTAGE COUNT")+
      ylab("COUNTIES")+
      geom_text(aes(label=percent))+
      theme( 
        #rotate the x and y axis to make them eligible
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(angle = 0))
  })
  
  output$treatment <- renderPlot({
    newtuberclosis %>% 
      count(treatment_outcome, sort = TRUE)%>%
      #make hiv status a factor
      mutate(treatment_outcome=factor(treatment_outcome)) %>% 
      #rearrange in descending order
      mutate(treatment_outcome= fct_reorder(treatment_outcome, n, .desc = TRUE)) %>% 
      ggplot(mapping = aes(x=treatment_outcome, y= n))+
      geom_col(
        aes(fill = treatment_outcome), show.legend = FALSE) +
      ggtitle("TREATMENT OUTCOME OF TB PATIENTS")+
      xlab("TREATMENT OUTCOME")+
      ylab("COUNT")+
      geom_text(aes(label = n, y =n+0.3))+
      paletteer::scale_fill_paletteer_d("rcartocolor::Pastel")+
      theme(
        plot.title = element_text(hjust = 0.8)
      )
  })
  
  output$success <- renderPlot({
    newtuberclosis %>% 
      select(treatment_outcome, county) %>% 
      filter(treatment_outcome=="C") %>% 
      count(county,treatment_outcome, sort = TRUE) %>% 
      #make county a factor
      mutate(county=factor(county)) %>% 
      #rearrange in descending order
      mutate(county= fct_reorder(county, n, .desc = TRUE)) %>% 
      slice_head(n=10) %>% 
      ggplot(mapping = aes(x=county, y= n))+
      geom_col(
        aes(fill = treatment_outcome), show.legend = FALSE) +
      ggtitle("TOP TEN COUNTIES WITH HIGHEST TREATMENT SUCCESS RATE")+
      xlab("COUNTIES")+
      ylab("NO. OF CURED PATIENTS")+
      geom_text(aes(label = n, y =n+0.3))+
      theme()
  })
  
  output$hostreatment <- renderPlot({
    ggplot(data = newtuberclosis) +
      geom_bar(mapping = aes(x = sector, fill = treatment_outcome))+
      facet_wrap( ~ treatment_outcome, scales = "free_y")+
      ggtitle("TREATMENT OUTCOME BASED ON HOSPITAL ATTENDED")+
      xlab("HOSPITAL ATTENDED")+
      ylab("COUNT")+
      theme(
        #rotate the x and y axis to make them eligible
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(angle = 0),
        plot.title = element_text(hjust = 0.3)
      )
  })

  
  #######AGE FACTOR########
  
  output$agefactor <- renderPlot({
    req(input$age_factor)
    newtuberclosis %>% 
      filter(age_on_registration %in% seq(input$age_factor[1], input$age_factor[2])) %>% 
      count(sub_county) %>% 
      # at least two cases, right?
      filter(n > 1) %>% 
      slice_head(n = 10) %>% 
      mutate(sub_county = fct_reorder(sub_county, n)) %>% 
      ggplot(mapping = aes(y= sub_county,n))+
      geom_point(size= 3, color="dodgerblue")+
      geom_segment(aes(y= sub_county, yend= sub_county, x=0, xend= n), size=2, color="red", alpha= 0.7)+
      labs(
        x= 'NO. OF CASES',
        y= 'SUB_COUNTY',
        title = 'TUBERCULOSIS CASES IN DIFFERENT SUB-COUNTIES AGAINIST AGE'
      )
  })
  
  output$zoneagefactor <- renderPlot({
    req(input$zoneage_factor)
    newtuberclosis %>%
      filter(age_on_registration %in% seq(input$age_factor[1], input$age_factor[2])) %>%
      count(health_facility) %>%
      # at least two cases, right?
      filter(n > 1) %>%
      slice_head(n = 10) %>%
      mutate(health_facility= fct_reorder(health_facility, n)) %>%
      ggplot(mapping = aes(y= health_facility,n))+
      geom_point(size= 3, color="chocolate")+
      geom_segment(aes(y= health_facility, yend= health_facility, x=0, xend= n), size=2, color="darkgreen", alpha= 0.7)+
      labs(
        x= 'NO. OF CASES',
        y= 'HEALTH FACILITY',
        title = 'TUBERCULOSIS CASES IN DIFFERENT HEALTH FACILITIES AGAINIST AGE'
      )
  })
  
  
  output$mymap <- renderLeaflet({
  
    
    leaflet(merged_tb_df) %>% 
      addTiles() %>% 
      addPolygons(color = "blue", weight = 1, dashArray = "3", fillColor = ~pal(tb_totals),
                  highlight= highlightOptions(
                    weight = 4,
                    color = "blue",
                    dashArray = "",
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight"="normal", padding="3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(position = c("bottomright"), pal,values = ~tb_totals)
    
  })
  
  
}


# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)



