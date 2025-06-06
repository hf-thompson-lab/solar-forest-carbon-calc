#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(shinyBS)
library(scales)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(title=div("Carbon Calculator: Solar Development on an Acre of Forest",
                         img(src='transparent background harvard forest logo.png', height="70px", width="70px"))),
    
    # Note about story map
    fluidRow(
      column(8,
             HTML("<p style='font-size: 20px'><b>For a guided walkthrough of the calculator, please <a href='https://storymaps.arcgis.com/stories/d3c57091f8ed4c5cbd25f59c8a4a623b/'>click here</a>.</b></p>")
            )
      ),
    
    br(),

    # Sidebar with user controls
    sidebarLayout(
        sidebarPanel(
            selectInput("projyear",
                        "Year of Site Clearing",
                        choices = c('2025' = 2025, '2030' = 2030, '2035' = 2035, '2040' = 2040, '2045' = 2045),
                        selected = 2030),
            bsPopover("projyear", "Year of Site Clearing",
                      "Year in which the forested site is cleared of trees and prepared for development",
                      placement='top'),
            
            selectInput("landreq",
                        "Capacity per Acre",
                        choices = c('1/3 MW (333 kW)' = 3, '1/4 MW (250 kW)' = 4, '1/5 MW (200 kW)' = 5, '1/6 MW (167 kW)' = 6, '1/7 MW (143 kW)' = 7, '1/8 MW (125 kW)' = 8, '1/9 MW (111 kW)' = 9),
                        selected = 4),
            bsPopover("landreq", "Capacity per Acre",
                      "How much power can be produced in an acre of land",
                      placement='top'),
            
            radioButtons("cstock",
                        "Forest Carbon Stock",
                        choices = c("Low (25th percentile, 25.9 Mg/ac)" = 25.9,
                                    "Average (50th percentile, 35.8 Mg/ac)" = 35.8,
                                    "Above Average (75th percentile, 46.7 Mg/ac)" = 46.7,
                                    "Exceptional (90th percentile, 57.4 Mg/ac)" = 57.4),
                        selected = 35.8),
            bsPopover("cstock", "Forest Carbon Stock",
                      "Amount of above-ground carbon in living trees relative to other forests in Massachusetts",
                      placement="top"),
            
            radioButtons("downed",
                         "Downed Wood",
                         choices = c("Off" = 0.0, "On (low)" = 0.05, "On (high)" = 0.15),
                         selected = 0.05),
            bsPopover("downed", "Carbon emissions from destruction of dead wood on the project site",
                      "<strong>On (low)</strong> assumes downed wood is 5% of live above-ground carbon<br><strong>On (high)</strong> assumes downed wood is 15% of live above-ground carbon",
                      placement="top"),
            
            radioButtons("soilemit",
                         "Soil Carbon Emissions",
                         choices = c("None", "Exponential Decay"),
                         selected = "Exponential Decay"),
            bsPopover("soilemit", "Shape of soil emissions, if any, from site clearing and development",
                      "<strong>None</strong> assumes no soil emissions<br><strong>Exponential Decay</strong> assumes a spike in emissions upon clearing followed by slower release to a stabilizing point",
                      placement="top"),
            
            selectInput("durable",
                         "Percent of Site Biomass in Durable Goods",
                         choices = c("None (0%)" = 0.0, "Low (10%)" = 0.1, "Average (20%)" = 0.2, "High (30%)" = 0.3, "Exceptionally High (50%)" = 0.5),
                        selected = 0.2),
            bsPopover("durable", "Percent of Site Biomass in Durable Goods",
                      "Percent of live aboveground biomass cleared on site that is not chipped or burned",
                      placement="top"),
            
            selectInput("gridintensity2050",
                        "Emissions from Electricity Generation in 2050",
                        choices = c("MA Decarbonization Roadmap (3.7 g/kWh)" = 0.0037, "~80% less than current (68.6 g/kWh)" = 0.0686, "Half of current (171.5 g/kWh)" = 0.1715, "~25% less than current (274 g/kWh)" = 0.274, "No change from current (343 g/kWh)" = 0.343)),
            bsPopover("gridintensity2050", "Overall trajectory of the electricity grid",
                      "This parameter is based on the <b>marginal emissions</b> of electricity generation and directly affects the <b>amount of CO2 emitted by electricity production that is displaced</b> by the solar project.<br>For example, a renewable project in an electricity grid that is already 95% decarbonized will have a lesser marginal impact than the same project on a grid that is only 20% decarbonized.<br>All options except \\'No change from current\\' assume a <b>linear decline</b> to the reduced level in 2050.",
                      placement="top"),
            
            radioButtons("curtailment",
                        "Curtailment",
                        choices = c("None", "Low (~5% in 2050)" = "Low", "Medium (~15% in 2050)" = "Medium", "High (~25% in 2050)" = "High"),
                        selected = "Low"),
            bsPopover("curtailment", "Percent reduction in usable solar energy",
                      "Curtailment happens when supply of renewable energy exceeds demand. The significance of curtailment is that the solar project is not displacing fossil fuels when curtailed, thereby reducing the emissions benefits of the project.<br><b>Low</b> curtailment is curtailment as specificed in the All Options pathway in the MA Decarbonization Roadmap, while <b>high</b> curtailment is in line with projections under a high renewables future. <b>Medium</b> curtailment is in the middle of these scenarios. In all scenarios, curtailment begins close to 0 in 2021 and increases over time due to the increased abundance of renewables on the grid.",
                      placement = "top"),
            
            radioButtons("gridfreeze",
                         "Freeze Grid Emissions in Project Year",
                         choices = c("No", "Yes")),
            bsPopover("gridfreeze", "Should the emissions from generating electricity be held constant during the project?",
                      "<b>No</b> means the emissions produced from generating electricity will decline through 2050 based on the user\\'s selection above. This assumes more renewables enter the grid when modeling the project\\'s impact through 2050.<br><b>Yes</b> means the emissions produced from generating electricity will be held constant at its value in the year the site is cleared. This option does not consider the incorporation of more renewables into the electricity grid in determining whether the project is a net carbon source or sink by 2050.",
                      placement="top")
        ),

        # Show a plot of the generated distribution
        # and a sentence summarizing the project carbon outcome by 2050
        mainPanel(
           plotOutput("carbon_calc"),
           htmlOutput("summary"),
           br(),
           h3("Non-carbon values of forests"),
           p("Carbon is not the only thing to consider when deciding whether to site solar on forestland. In addition to carbon sequestration, some of the values of forests include:"),
           h4("Recreation"),
           fluidRow(
             column(4,
                    p("Forests provide spaces to explore, play, learn, and recharge in natural environments, but access to forests for recreation is not equitable. Before replacing forests with solar arrays, consider the current abundance, quality, and accessibility of forested recreation sites, and the potential impact of solar development on this dimension of environmental justice."),
                    em("Photos by Jonathan Thompson")
             ),
             column(8,
                    img(src="rooster.png",
                        alt="Rooster the dog sits in front of fence closing off land previously used as a trail",
                        width="36%", style="float:right"),
                    img(src="trail_diversion.png",
                        alt="Sign on a fence points to new trail location after the trail was converted to solar",
                        width="64%", style="float:right")
             )
           ),
           fluidRow(
             h4("Biodiversity"),
             p("Globally, forests contain 80% of amphibian species, 75% of bird species, and 68% of the world's mammal species. The continued existence of healthy forests is essential for sustaining biodiversity, which has been plumetting for decades due to climate change and habitat loss in what is referred to as the 'sixth mass extinction.' The biodiversity and climate crises are inseparable and mutually reinforcing. Therefore, destroying forests for development of any kind, including clean energy, must be done extremely carefully if it must be done at all."),
             img(src="Simes_2008_Ovenbird_UNK.jpg",
                 alt="Ovenbird perched on a branch",
                 width="31%", style="float:right"),
             img(src="Petersham_2014_EasternNewt_CH.jpg",
                 alt="Eastern newt on forest floor with a hemlock cone",
                 width="33%", style="float:right"),
             img(src="ProspectHill_2013_Bear_CH.jpg",
                 alt="Black bear walks through a forest",
                 width="33%", style="float:right")
             ),
           fluidRow(em("Harvard Forest Photographs by Clarisse Hart and Unknown Photographer")),
           fluidRow(
             h4("Water"),
             img(src="SlabCity_2018_SwiftRiver2_2018_DRF.jpg",
                 alt="River running through a forest in summer",
                 height="300", style="float:right"),
             p("Because forests are full of plants -- specifically trees -- with root systems throughout the soil, they filter water, naturally purifying it, slow down the movement of water through soil, preventing erosion, and absorb water during heavy precipitation, helping to mitigate flooding. The potential impact of deforestation on erosion, water quality, and flood risk should be considered before converting forest into solar arrays."),
             em("Harvard Forest Photograph by David R. Foster")
           ),
           br(),
           br(),
           p("Contact: Lucy Lee, lucylee@fas.harvard.edu"),
           br()
        )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    data <- reactive({
      #### Set up ####
      # Build data frame to construct data based on user input
      dat <- data.frame("year" = 2021:2050,
                        "period" = 0:29)
      
      # Factor to convert C to CO2e
      co2e <- 3.666667
      
      #### Calculate foundational data ####
      # Calculate grid intensity over time using marginal emission rate
      marginal.emissions <- 758 * 0.000453592  # actual 2021 in Mg CO2 / MWh (reported value = 758 lbs CO2 / MWh) -- grid emissions input tied to this value
      grid.intensity.start <- marginal.emissions
      grid.intensity.end <- as.numeric(input$gridintensity2050)
      grid.intensity.decay <- (grid.intensity.start - grid.intensity.end) / (nrow(dat)-1)   # Linear
      dat$grid.intensity <- grid.intensity.start - (grid.intensity.decay * dat$period)
      
      # Get usable PV each year (from Nathan Oalican's analysis) based on user selection
      # High UPV = All Options pathway, Medium = difference b/w high and medium, Low = original medium
      # These values are the proportion of PV generated that is NOT curtailed (i.e., what is usable -> UPV)
      # After joining correct scenario, change colname to "upv" so no need for conditional later
      usable_pv <- read.csv("www/upv_edit.csv", stringsAsFactors = F)   # Use version of UPV that has new scenario between Nathan's high and medium
      if (input$curtailment == "None") {
        dat$upv <- 1
      } else if (input$curtailment == "Low") {
        dat <- left_join(dat, usable_pv[, c("year", "upv_high")], by = "year")   # High UPV if curtailment is low
        colnames(dat)[names(dat) == "upv_high"] <- "upv"
      } else if (input$curtailment == "Medium") {
        dat <- left_join(dat, usable_pv[, c("year", "upv_medium")], by = "year")
        colnames(dat)[names(dat) == "upv_medium"] <- "upv"
      } else if (input$curtailment == "High") {
        dat <- left_join(dat, usable_pv[, c("year", "upv_low")], by = "year")  # Using Nathan's "medium" curtailment as the "high" curtailment here
        colnames(dat)[names(dat) == "upv_low"] <- "upv"
      }
      
      # Calculate AGC over time
      # Remember that 'cstock' is the forest live AGC, from which we calculate total live forest C,
      #including the live belowground carbon (roots)
      cseq <- 1.2 * co2e  # Mg CO2e / ac / yr
      dat$agc <- (as.numeric(input$cstock) * co2e) + (cseq * dat$period)   # Mg CO2e / ac
      
      # Calculate total live forest carbon -- this is 1.25x the live AGC (adds living belowground like roots)
      dat$livec <- dat$agc * 1.25   # Mg CO2e / ac
      
      # Calculate BGC over time 
      soilseq <- 0.08 * co2e   # Mg C / ac / yr from Finzi et al converted to CO2e
      dat$bgc <- (52.01 * co2e) + (soilseq * dat$period)  # 52.01 is HF hardwood soil C from Finzi et al
      
      # Calculate downed wood over time
      dat$downedwood <- dat$agc * as.numeric(input$downed)  # Mg CO2e / ac
      
      # Calculate clearing emissions at each year - includes the downed wood, live AGC (some of which is put
      # into durable goods), and live BGC (total live C minus live AGC)
      # Does not include soil emissions!
      dat$clearing.emissions <- (dat$agc * (1 - as.numeric(input$durable))) + dat$downedwood + (dat$livec - dat$agc)   # Mg CO2e / ac
      
      # Calculate emissions from solar panel production over time
      # CF updated from 0.2 to 0.1302 on 6/2025 based on this report from DOER:
      # https://www.mass.gov/info-details/solar-carve-out-and-solar-carve-out-ii-minimum-standards-and-market-information
      cf <- 0.1302
      annual.pv.energy <- 1 * 8760 * cf   # MWh / MW / year -- annual MWh output of 1 MW plant given cf
      
      # Calculate annual PV energy production for 1 MW project
      # And multiply by the proportion of PV that is usable
      annual.pv.energy.per.ac <- (1 / as.numeric(input$landreq)) * annual.pv.energy   # MWh / ac / yr
      dat$annual.pv <- annual.pv.energy.per.ac * dat$upv
      
      # Calculate grid CO2 emissions for the equivalent amount of energy
      # In other words, this is the amount of grid C offset by PV production
      # Given in Mg / ac / yr because our calculator is based on the perspective of 1 acre of land
      dat$grid.co2.displaced.ann <- dat$grid.intensity * dat$annual.pv   # Mg CO2 / ac / yr
      
      #### Calculate project data ####
      # Subset to just the years where project exists
      project.year <- as.numeric(input$projyear)
      dat <- dat[dat$year >= project.year, ]
      
      # Recalculate period for the project time period
      dat$period <- 0:(nrow(dat)-1)
      
      # Recalculate grid intensity and grid co2 displaced if the user
      # has chosen to use jet fuel c intensity to represent non-electric sector emissions
      # This assumes any excess solar produced can be directed toward other uses
      
      
      # Recalculate grid intensity and grid co2 displaced if the user
      # has chosen to freeze grid intensity in the project year
      if (input$gridfreeze == 'Yes'){
        # Retrieve the grid intensity for that year and apply to all years of the project
        grid.intensity.proj.year <- dat[1, 'grid.intensity']
        dat$grid.intensity <- grid.intensity.proj.year
        
        # Recalculate grid co2 displaced with stable grid intensity
        dat$grid.co2.displaced.ann <- dat$grid.intensity * dat$annual.pv
      }
      
      # Calculate soil emissions due to project construction based on user selection
      if (input$soilemit == "Exponential Decay"){
        # Calculate belowground emissions beginning in project year
        dat[1, 'soil.emissions'] <- dat[1, 'bgc'] * 0.2   # 20% emission in project year
        dat[1, 'soil.remaining'] <- dat[1, 'bgc'] * 0.8   # Remaining soil c left after emissions
        
        # For years after the project year, logarithmic decay - 2.5% of the prior year's remaining soil C
        for (i in 2:nrow(dat)){
          # 2.5% of the remaining soil c from prior year
          dat[i, 'soil.emissions'] <- 0.025 * dat[i-1, 'soil.remaining']
          # remaining soil c is initial pool minus sum of emissions
          dat[i, 'soil.remaining'] <- dat[1, 'bgc'] - sum(dat[1:i, 'soil.emissions'])  
        }
      } else if (input$soilemit == "None"){
        dat$soil.emissions <- 0
      }
      
      #### Calculate an annual project co2e ####
      # Year of clearing - clearing emissions above and below ground, avoided cseq
      dat[1, 'proj.co2e.annual'] <- dat[1, 'clearing.emissions'] + cseq + dat[1, 'soil.emissions']
      
      # Between clearing and operation, cseq is not occurring - assume 2 years for this
      dat[2, 'proj.co2e.annual'] <- cseq + dat[2, 'soil.emissions']
      dat[3, 'proj.co2e.annual'] <- cseq + dat[3, 'soil.emissions']
      
      # In operation, annual balance is cseq avoided and grid C displaced
      dat[4:nrow(dat), 'proj.co2e.annual'] <- cseq - dat[4:nrow(dat), 'grid.co2.displaced.ann'] + dat[4:nrow(dat), 'soil.emissions']
      
      # cumsum the annual column to get continuously accumulative C balance
      dat$proj.co2e.cumulative <- cumsum(dat$proj.co2e.annual)
      
      # Subset data for plotting
      plotdat.line <- dat[, c('year', 'proj.co2e.cumulative')]
      return(plotdat.line)
    })
  
    # Generate carbon plot
    output$carbon_calc <- renderPlot({
      # Retrieve reactive data
      plotdat.line <- data()
      
      # Create data for leader line
      project.year <- as.numeric(input$projyear)
      p <- plotdat.line[1, 'proj.co2e.cumulative']  # get the value for the first year
      df <- data.frame(x = c(project.year, project.year),
                       y = c(0, p))
      
      # Get upper and lower y lims for drawing arrows
      y.top <- max(plotdat.line[, 2])
      y.bot <- min(plotdat.line[, 2])
      
      # Base plot -- elements that do not change
      g <- ggplot2::ggplot() +
        geom_line(data=plotdat.line, aes(x=year, y=proj.co2e.cumulative), linewidth=1.25, colour='blue') + 
        geom_line(data=df, aes(x=x, y=y), linewidth=1.25, colour='blue')+
        labs(x='Year', y='CO2e Mg / ac') +
        geom_hline(yintercept=0, linetype="dashed", color="darkgrey") +
        coord_cartesian(xlim=c(2023, 2050)) +
        scale_x_continuous(breaks=c(2025, 2030, 2035, 2040, 2045, 2050)) +
        ggtitle("Cumulative Carbon Balance") +
        theme_bw() +
        theme(plot.title = element_text(size = 16, face = 'bold'),
              axis.title = element_text(size = 14, face = 'bold'),
              axis.text = element_text(size = 12))
      
      # Conditionally adjust y-axis limits and arrow placement/text
      # Get y-axis breaks to compare to min (y.bot) and max (y.top) values present
      y.breaks <- as.numeric(na.omit(layer_scales(g)$y$break_positions()))
      break.gap <- abs(max(y.breaks) - min(y.breaks)) / (length(y.breaks)-1)  # Get distance between each break -- this method doesn't assume # of breaks present
      # If y.top is positive but there is no y-axis break above zero...
      if (y.top > 0 & max(y.breaks) <= 0) {
        # Create a new break above the max break present
        top.break <- max(y.breaks) + break.gap
        
        # Set breaks and limits to include the new top break
        g <- g + scale_y_continuous(breaks=c(y.breaks, top.break),
                                    limits = c(NA, top.break),
                                    labels=comma)
        
        # Retrieve min/max of new breaks for formatting arrows/labels
        y.breaks.new <- as.numeric(na.omit(layer_scales(g)$y$break_positions()))
        y.break.min <- min(y.breaks.new)
        y.break.max <- max(y.breaks.new)
        
        # Get midway point for arrow labels positions
        mid.up <- round(y.break.max / 2, 0)
        mid.low <- round(y.break.min / 2, 0)
        
        # Check positive values -- if small, abbreviate arrow label
        if (y.break.max <= 500) {
          emit.label <- "C emission"
        } else {
          emit.label <- "Carbon emission"
        }
        
        # Check negative values -- if small, abbreviate arrow label
        if (y.break.min >= -500) {
          remove.label <- "C removal"
        } else {
          remove.label <- "Carbon removal"
        }
        
        # If values go below zero, draw the carbon removal arrow/label
        if (y.bot <= 0) {
          g <- g + geom_segment(aes(x=2023, y=0, xend=2023, yend=y.break.min),
                                linewidth=2, color="darkgreen", arrow=arrow()) +
            geom_text(aes(x=2024, y=mid.low, label=remove.label),
                      angle=90, color="darkgreen", fontface="bold")
        }
        
        # Add the carbon emission arrow/label
        g <- g + geom_segment(aes(x=2023, y=0, xend=2023, yend=y.break.max),
                              linewidth=2, color="red", arrow=arrow()) +
          geom_text(aes(x=2024, y = mid.up, label=emit.label),
                    angle=90, color="red", fontface="bold")
        
        # If the min value is negative but there are no breaks below zero...
      } else if (y.bot < 0 & min(y.breaks) >= 0){
        # Create a new break below the minimum break
        bot.break <- min(y.breaks) - break.gap
        
        # Set the breaks and limits to include the new bottom break
        g <- g + scale_y_continuous(labels=comma,
                                    breaks=c(bot.break, y.breaks),
                                    limits=c(bot.break, NA))
        
        # Get new min/max breaks for placing arrows/labels
        y.breaks.new <- as.numeric(na.omit(layer_scales(g)$y$break_positions()))
        y.break.min <- min(y.breaks.new)
        y.break.max <- max(y.breaks.new)
        
        # Get midway point for arrow labels positions
        mid.up <- round(y.break.max / 2, 0)
        mid.low <- round(y.break.min / 2, 0)
        
        # Check positive values -- if small, abbreviate arrow label
        if (y.break.max <= 500) {
          emit.label <- "C emission"
        } else {
          emit.label <- "Carbon emission"
        }
        
        # Check negative values -- if small, abbreviate arrow label
        if (y.break.min >= -500) {
          remove.label <- "C removal"
        } else {
          remove.label <- "Carbon removal"
        }
        
        # If values go below zero, draw the carbon removal arrow/label
        if (y.bot <= 0) {
          g <- g + geom_segment(aes(x=2023, y=0, xend=2023, yend=y.break.min),
                                linewidth=2, color="darkgreen", arrow=arrow()) +
            geom_text(aes(x=2024, y=mid.low, label=remove.label),
                      angle=90, color="darkgreen", fontface="bold")
        }
        
        # Add the carbom emission arrow/label
        g <- g + geom_segment(aes(x=2023, y=0, xend=2023, yend=y.break.max),
                              linewidth=2, color="red", arrow=arrow()) +
          geom_text(aes(x=2024, y = mid.up, label=emit.label),
                    angle=90, color="red", fontface="bold")
        
        # If neither of the above conditions are met, the existing breaks are OK
      } else {
        # Add y-axis and carbon emission arrow/label
        # Note that in this case the arrow placement is based on the data values,
        # and not the plot limits as in the previous two conditions
        g <- g + scale_y_continuous(labels=comma, breaks=y.breaks) +    # Adding breaks=y.breaks ensures all breaks are labeled
          geom_segment(aes(x=2023, y=0, xend=2023, yend=y.top),
                       linewidth=2, color="red", arrow=arrow()) +
          geom_text(aes(x=2024, y = mid.up, label=emit.label),
                    angle=90, color="red", fontface="bold")
        
        # Get midway point on arrows for labels
        mid.up <- round(y.top / 2, 0)
        mid.low <- round(y.bot / 2, 0)
        
        # Check positive values -- if small, abbreviate arrow label
        if (y.top <= 500) {
          emit.label <- "C emission"
        } else {
          emit.label <- "Carbon emission"
        }
        
        # Check negative values -- if small, abbreviate arrow label
        if (y.bot >= -500) {
          remove.label <- "C removal"
        } else {
          remove.label <- "Carbon removal"
        }
        
        # If values go below zero, draw the carbon removal arrow/label
        if (y.bot <= 0) {
          g <- g + geom_segment(aes(x=2023, y=0, xend=2023, yend=y.bot),
                                linewidth=2, color="darkgreen", arrow=arrow()) +
            geom_text(aes(x=2024, y=mid.low, label=remove.label),
                      angle=90, color="darkgreen", fontface="bold")
        }
      }
      
      # Show the plot
      plot(g)
    })
    
    # Generate summary sentence
    output$summary <- renderText({
      # Retrieve reactive data
      plotdat.line <- data()
      
      # Translate numerical cstock into the qualitative description
      # If this isn't done, the number will print, not the UI label
      cstock <- ifelse(input$cstock == 25.9, "Low",
                       ifelse(input$cstock == 35.8, "Average",
                              ifelse(input$cstock == 46.7, "Above average",
                                     ifelse(input$cstock == 57.4, "Exceptional", NA))))
      
      # Translate grid intensity in 2050 to qualitative terms too
      gridc <- ifelse(input$gridintensity2050 == 0.0037, "On track to achieve the MA Decarbonization Roadmap All Options pathway by 2050",
                          ifelse(input$gridintensity2050 == 0.0686, "~80% less than current by 2050",
                             ifelse(input$gridintensity2050 == 0.1715, "Half of current by 2050",
                                    ifelse(input$gridintensity2050 == 0.274, "~25% less than current",
                                           ifelse(input$gridintensity2050 == 0.343, "No change from current", NA)))))
      
      gridf <- ifelse(input$gridfreeze == "Yes", "Held constant at the emission rate of the year the site is cleared",
                      ifelse(input$gridfreeze == "No", "Continue to decline through 2050 as renewable energy sources enter the grid", NA))
      
      # Same for durable goods fraction
      dgf <- ifelse(input$durable == 0.0, "None (0%)",
                    ifelse(input$durable == 0.1, "Low (10%)",
                           ifelse(input$durable == 0.2, "Average (20%)",
                                  ifelse(input$durable == 0.3, "High (30%)",
                                         ifelse(input$durable == 0.5, "Exceptionally high (50%)", NA)))))
      
      # And soil emission
      soils <- ifelse(input$soilemit == "None", "Do not occur",
                      ifelse(input$soilemit == "Exponential Decay", "Spike during clearing then release slowly", NA))
      
      # And downed wood -- MAKE REFLECT HIGH/LOW SELECTION!
      dw <- ifelse(input$downed == 0.0, "Not present on site",
                   ifelse(input$downed == 0.05, "5% of living above ground carbon",
                          ifelse(input$downed == 0.15, "15% of living above ground carbon", NA)))
      
      # And capacity per ac -- behind the scenes this is really the ac/MW, but to the user it is framed
      # as MW/ac -- need to translate values here
      cap <- ifelse(input$landreq == 3, '1/3 MW (333 kW)',
                    ifelse(input$landreq == 4, '1/4 MW (250 kW)',
                           ifelse(input$landreq == 5, '1/5 MW (200 kW)',
                                  ifelse(input$landreq == 6, '1/6 MW (167 kW)',
                                         ifelse(input$landreq == 7, '1/7 MW (143 kW)',
                                                ifelse(input$landreq == 8, '1/8 MW (125 kW)',
                                                       ifelse(input$landreq == 9, '1/9 MW (111 kW)', NA)))))))
      
      # And curtailment
      curt <- ifelse(input$curtailment == "Low", "Low, with just under 5% of solar energy curtailed by 2050",
                     ifelse(input$curtailment == "Medium", "Moderate, with 15% of solar energy curtailed by 2050",
                            ifelse(input$curtailment == "High", "High, with more than 25% of solar energy curtailed by 2050",
                                   ifelse(input$curtailment == "None", "None", NA))))
      
      # Get carbon status in 2050 -- could also report windows of carbon sink/source
      # Get the final year of data (for summary sentence)
      value2050 <- plotdat.line[nrow(plotdat.line), 2]
      status2050 <- ifelse(value2050 < 0, "sink",
                           ifelse(value2050 > 0, "source", NA))
      add_remove <- ifelse(value2050 < 0, "removing", "adding")
      formatted_value2050 <- abs(round(value2050, 1))
      car_equiv <- abs(round(formatted_value2050 / 4.6, 0))
      
      paste0("Using the following selections:\n<ul><b>Year of site clearing:</b> ", input$projyear, "</ul>",
             "<ul><b>Capacity per acre:</b> ", cap, "</ul>",
             "<ul><b>Forest carbon stock:</b> ", cstock, "</ul>",
             "<ul><b>Downed wood:</b> ", dw, "</ul>",
             "<ul><b>Soil emissions:</b> ", soils, "</ul>",
             "<ul><b>Percent of site biomass put in durable goods:</b> ", dgf, "</ul>",
             "<ul><b>MA electricity decarbonization trajectory:</b> ", gridc, "</ul>",
             "<ul><b>Curtailment:</b> ", curt, "</ul>",
             "<ul><b>MA electricity emissions:</b> ", gridf, "</ul>",
             "The conversion of the acre of forestland to solar arrays results in a <b>net carbon ", status2050, "</b> by 2050, <b>", add_remove," ", formatted_value2050, " metric tons</b> of carbon dioxide in the atmosphere. This is roughly equivalent to the annual emissions of <b>", car_equiv, " cars</b> (<a href='https://www.epa.gov/greenvehicles/greenhouse-gas-emissions-typical-passenger-vehicle'>source</a>).")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
