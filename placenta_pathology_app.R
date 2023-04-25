#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load libraries 
library(shiny)
library(shinyjs)
library(devtools)
library(yaml)
library(data.tree)
library(shinyTree)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(bsplus)
library(jsonlite)
library(shinymanager)
library(tidyr)
library(shinyBS)



# This formats as yaml and can be used for data.tree
yaml<-"
name: Placental pathology tree
Pathophysiologic category:
  1_Maternal_uteroplacental_vascular_disorders:
    A. Maternal uteroplacental-vascular maldevelopment:
      i. superficial implantation:
        a. persistent muscularization, basal plate arteries:
          True: 1
        b. increased placental site giant cells:
          True: 1
      ii. decidual arteriopathy:
        a. mural hypertrophy, decidual arterioles:
          True: 1
        b. hypertrophic arteriopathy, decidual arterioles:
          True: 1
        c. fibrinoid necrosis of decidual arterioles with or without foam cells:
          True: 1
      iii. morbidly adherent placenta:
        a. adherent myometrial fibers, basal plate:
          True: 1
        b. placenta accreta:
          True: 1
        c. placenta increta:
          True: 1
        d. placenta percreta:
          True: 1
        e. cesarean scar pregnancy:
          True: 1
      iv. excessive immature (transitional) extravillous trophoblast:
        a. trophoblast island:
          True: 1
        b. chorionic cyst:
          True: 1
        c. microcysts, membranes or basal plate:
          True: 1
    B. Maternal uteroplacental-vascular malperfusion:
      i. maternal vascular malperfusion, global:
        a. accelerated villous maturation:
          True: 1
        b. distal villous hypoplasia, focal:
          True: 1
        c. distal villous hypoplasia, diffuse:
          True: 1
      ii. maternal vascular malperfusion, segmental:
        a. villous infarct, recent:
          True: 1
        b. villous infarct, remote:
          True: 1
    C. Maternal uteroplacental-vascular loss of Integrity:
      i. acute abruption, arterial (“abruptio placentae”):
          True: 1
      ii. subacute abruption, arterial (subacute “abruptio placentae”):
          True: 1
      iii. acute marginal abruption, venous:
          True: 1
      iv. chronic marginal abruption, venous:
          True: 1
    D. Maternal uteroplacental-vascular disorder, pathogenesis incompletely understood:
      i. increased perivillous fibrin:
        a. perivillous fibrin plaque:
          True: 1
        b. massive perivillous fibrin deposition (“maternal floor infarction”):
          True: 1
      ii. atypical intervillous hemorrhage:
        a. rounded intraplacental hematoma (“infarction-hematoma”):
          True: 1
        b. massive subchorial thromohematoma:
          True: 1
        c. subchorionic intervillous thrombus:
          True: 1
        d. basal intervillous thrombus:
          True: 1
        e. basal plate plaque:
          True: 1
  2_Fetal_stromal-vascular_disorders:
    A. Fetal stromal-vascular maldevelopment:
      i. villous capillary lesions:
        a. chorangioma:
          True: 1
        b. chorangiosis:
          True: 1 
        c. multifocal chorangiomatosis:
          True: 1 
      ii. dysmorphic villi:
        a. irregular villous contour OR trophoblast inclusions:
          True: 1
        b. proximal-distal villous discordance:
          True: 1
        c. abnormal distal villous vascular pattern:
          True: 1
      iii. delayed villous maturation, diabetic type:
        True: 1 
    B. Fetal stromal-vascular malperfusion:
      i. fetal vascular malperfusion, global:
        a. avascular villi, small foci:
          True: 1 
        b. recent villous stromal-vascular karyorrhexis, small foci:
          True: 1
        c. intramural fibrin, large fetal vessel:
          True: 1
        d. venous ectasia, chorionic plate and major stem villi:
          True: 1
        e. delayed villous maturation, umbilical cord obstructive type:
          True: 1
      ii. fetal vascular malperfusion, segmental:
        a. avascular villi, large-intermediate foci:
          True: 1
        b. recent villous stromal-vascular karyorrhexis, large-intermediate foci:
          True: 1
        c. fetal large vessel thrombus:
          True: 1
        d. fetal stem vessel obliteration:
          True: 1
    C. Fetal stromal-vascular loss of integrity:
        a. intervillous thrombus:
          True: 1
        b. villous stromal edema:
          True: 1
        c. villous stromal hemorrhage:
          True: 1
  3_Inflammatory_processes:
    A. Inflammatory process, infectious:
      a. acute chorioamnionitis, infectious, maternal inflammatory response:
        True: 1
      b. acute chorioamnionitis, infectious, fetal inflammatory response:
        True: 1
      c. acute villitis OR intervillositis, infectious:
        True: 1
      d. chronic villitis, infectious:
        True: 1
      e. chronic intervillositis, infectious:
        True: 1
      f. chronic deciduitis, infectious:
        True: 1
    B. Inflammatory process, non-infectious:
      a. chronic villitis, low grade, noninfectious:
        True: 1
      b. chronic villitis, high grade, noninfectious:
        True: 1
      c. basal villitis, noninfectious:
        True: 1
      d. chronic histiocytic intervillositis, noninfectious:
        True: 1
      e. chronic chorioamnionitis, noninfectious:
        True: 1
      f. lymphoplasmacytic deciduitis, noninfectious:
        True: 1
      g. diffuse chronic deciduitis, noninfectious:
        True: 1
      h. fetal eosinophilic OR T cell chorionic vasculitis, noninfectious:
        True: 1
      i. fetal chorionic histiocytic hyperplasia, noninfectious:
        True: 1
  4_Malformation-deformation:
    A. Abnormal early placental development:
      i. monochorionic twinning:
        True: 1
      ii. amniotic deformity-adhesion mutilation (ADAM) sequence:
        True: 1
      iii. abnormal placental surface chorionic vessel network:
        True: 1
    B. Abnormal placental weight:
      a. large for gestational age placenta:
        True: 1
      b. small for gestational age placenta:
        True: 1
      c. increased fetoplacental weight ratio:
        True: 1
      d. decreased fetoplacental weight ratio:
        True: 1
    C. Abnormal placental shape:
      a. irregular placental contour OR accessory lobes:
        True: 1
      b. bilobate placenta:
        True: 1
      c. short chorion (chorion regression syndrome):
        True: 1
      d. placenta membranacea:
        True: 1
    D. Abnormal umbilical cord:
      i. abnormal umbilical cord length:
        a. excessively long umbilical cord:
          True: 1
        b. excessively short umbilical cord:
          True: 1
      ii. abnormal umbilical cord conformation:
        a. thin umbilical cord:
          True: 1
        b. hypercoiled umbilical cord:
          True: 1
        c. umbilical cord stricture:
          True: 1
      iii. abnormal umbilical cord insertion site:
        a. membranous insertion of umbilical cord:
          True: 1
        b. marginal insertion of umbilical cord:
          True: 1
        c. peripheral insertion of umbilical cord:
          True: 1
        d. furcate insertion of umbilical cord:
          True: 1
        e. tethered insertion of umbilical cord:
          True: 1
      iv. umbilical vascular anomalies:
       a. single umbilical artery (or hypoplastic second umbilical artery):
         True: 1
       b. persistent right umbilical vein:
         True: 1
       c. umbilical vessel aneurysm:
         True: 1
       d. angiomyxoma, umbilical cord:
         True: 1
       e. venous malformation OR arteriovenous malformation, umbilical cord:
         True: 1
       f. vitelline vascular proliferation, umbilical cord:
         True: 1
      v. umbilical cord remnants/cysts:
       a. allantoic duct remnant OR cyst:
         True: 1
       b. omphalomesenteric (vitelline) duct remnant OR cyst:
         True: 1
       c. squamous inclusion cyst, umbilical cord:
         True: 1
       d. pseudocyst, umbilical cord:
         True: 1
    E. Heterotopias:
      a. adrenal rest:
        True: 1
      b. hepatic rest:
        True: 1
    F. Placental tumors:
      a. teratoma:
        True: 1
      b. yolk sac tumor:
        True: 1
      c. intraplacental choriocarcinoma:
        True: 1
  5_Disruptions:
    A. Findings associated with antenatal membrane rupture:
      i. amnion nodosum (oligohydramnios sequence):
        True: 1
      ii. amniotic band syndrome:
        True: 1
      iii. late amnion rupture with amnion-chorion dehiscence:
        True: 1
    B. Rupture of large fetal vessel (ruptured vasa previa):
          True: 1
    C. Toxic exposure:
      i. recent meconium exposure:
        True: 1
      ii. prolonged meconium exposure:
        True: 1
      iii. umbilical cord ulceration, toxic:
        True: 1
  6_Genomic_disorders:
    A. Findings associated with genomic structural abnormalities:
      i. findings associated with chromosomal abnormalities, global (fetoplacental):
        True: 1
      ii. findings associated with chromosomal abnormalities, confined placental mosaicism:
        True: 1
      iii. findings associated with single gene defects:
        True: 1
    B. Findings associated with parental genomic imbalance (imprinting):
      i. complete hydatidiform mole:
        True: 1
      ii. androgenetic biparental mosaic chimerism:
        True: 1
      iii. Beckwith-Weidemann syndrome:
        True: 1
      iv. uniparental disomy:
        True: 1
  7_Epigenomic_disorders:
    True: 1
 "


# tests on class
osList <- yaml.load(yaml)
osNode <- as.Node(osList)
#print(osNode, "lesion")


#lock
inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"

# data.frame with credentials info
credentials <- data.frame(
  user = "placenta",
  password = "patho597",
  stringsAsFactors = FALSE
)

# load data
## created a csv file that contains the name of each disorder (column name) and the corresponding definition (row)
#check.names= false allows R to read "special characters"
my_data <- read.csv("placentaData_ShinyTree4.csv", check.names = FALSE)
as.data.frame(my_data)
colnames(my_data)
head(my_data)

#create list of images 
files  <- list.files("www")

#text for home page
p1 <- "The placenta atlas tool is organized in a hierarchical tree structure with branching dependent elements (see outline below).  Major pathophysiologic categories are divided into subcategories, which in turn each have one or more diagnostic headers that group lesions based on a single unifying concept.  Under each diagnostic header are the individual lesions and the specific microscopic findings that encompass the common core of placental pathology as it is presently understood."

#hover 
js <- {HTML("
$(document).on('shiny:connected', function(event) {
  $('#tree')
      .on('select_node.jstree', function (e, data) {
        var $node = $('#' + data.node.id);
        var x = $node.position().top,
             y = $node.position().left;
      
        Shiny.setInputValue('hoverednode', data.node.text);
        var tooltipSpan = $('#hoverinfo')[0];
        tooltipSpan.style.opacity = 1;
        tooltipSpan.style.top = x + 'px';
        tooltipSpan.style.left = y + 160 + 'px';
        
      })
      .on('select_node.jstree', function(e, data) {
        var tooltipSpan = $('#hoverinfo1')[0];
        tooltipSpan.style.opacity = 0;
        
        
        Shiny.setInputValue('hoverednode1', data.node.text);
        var tooltipSpan = $('#hoverinfo1')[0];
        tooltipSpan.style.opacity = 1;
        tooltipSpan.style.top = x + 'px';
        tooltipSpan.style.left = y + 160 + 'px';  
        
      });
});")}


#end of hover


#look up data table
testing <- my_data[c(2,5,6),]
rownames(testing) <- c('synonyms', 'keyclinicalterms', 'keypathology')

drop <- c("type")
testing2 = testing[,!(names(testing) %in% drop)]
tested<- t(testing2)
lookup_table<- as.data.frame(tested)
lookup_table <- cbind(Preferred_term= rownames(lookup_table), lookup_table)
rownames(lookup_table) <- NULL
colnames(lookup_table) <- c("Preferred Term", "Synonyms", "Keywords Clinical Associations", "Keywords Pathophysiology")


ui <- secure_app(head_auth = tags$script(inactivity),
                 
                 
                 
                 navbarPage("Placenta Atlas Tool", id = "navbarID", theme = shinytheme("cosmo"),
                            
                            #first tab panel
                            tabPanel("Home",
                                     fluidPage(titlePanel(title=h3("Placenta Atlas Tool", style = "font-size: 45px")),
                                               # titlePanel(title=h4("The placenta atlas tool is a systemic approach for classifying placental lesions that is suitable for worldwide implementation.", style = "font-size: 25px")),
                                               
                                               column(width= 8,
                                                      img(src = "a. persistent muscularization, basal plate arteries_a.png", height="20%", width="100%")
                                                      
                                               ),
                                               column(width= 4,
                                                      
                                                      
                                                      tabsetPanel( type="tab",
                                                                   tabPanel("About the app", 
                                                                            tags$span(
                                                                              class = "more",
                                                                              headerPanel(""),
                                                                              HTML(paste("<p>",  "The placenta atlas tool is a systemic approach for classifying placental lesions that is suitable for worldwide implementation.", "</p>",
                                                                                         
                                                                                         "<p>",  "This tool is organized in a hierarchical tree structure with the following branching dependent elements:", "</p>",
                                                                                         "<p>", "<b>", "1, 2, 3, … = pathophysiologic categories", "</b>", "</p>",
                                                                                         "<p>", "<b>","A, B, C, … = subcategories ", "</b>","</p>",
                                                                                         "<p>", "<b>","I, ii, iii, … = diagnostic headers/ unifying concepts", "</b>","</p>",
                                                                                         "<p>", "<b>","a, b, c, … = individual lesions/ specific microscopic findings", "</b>", "<a href='#TESTING'> ...read more </a>", "</p>"
                                                                                         
                                                                                         
                                                                                         
                                                                                         
                                                                              )) 
                                                                            )
                                                                   )
                                                                   
                                                      )
                                                      
                                               ),
                                               column(width = 12,
                                                      headerPanel(""),
                                                      headerPanel(""),
                                                      headerPanel(""),
                                                      HTML("<h2 id = TESTING> </h2>"),
                                                      HTML(paste(           
                                                        "<p style='font-size: 18px'>", "<u>","<b>", "The Pathophysiologic Categories", "</b>", "</u>","</p>", 
                                                        "<p style='font-size: 16px'>", "Major pathophysiologic categories are divided into subcategories, which in turn each have one or more diagnostic headers that group lesions based on a single unifying concept.  Under each diagnostic header are the individual lesions and the specific microscopic findings that encompass the common core of placental pathology as it is presently understood.","</p>",
                                                        "<p style='font-size: 16px'>", "The individual lesions/specific microscopic findings use 2016 Amsterdam consensus diagnostic terminology as defined in the original Amsterdam consensus manuscript and the subsequent, more comprehensive 2018 textbook","<a href='", "https://pubmed.ncbi.nlm.nih.gov/27223167/", "' target='_blank''>", "(1,", "</a>", "<a href='", "https://link.springer.com/book/10.1007/978-3-319-97214-5?noAccess=true", "' target='_blank''>", " 2).", "</a>", "</p>",
                                                        
                                                        "<p style='font-size: 16px'>", "The overall organization of the atlas follows that used in our recent textbook", "<a href='", "https://assets.cambridge.org/97813166/32536/frontmatter/9781316632536_frontmatter.pdf", "' target='_blank''>", "(3).", "</a>" ," The placental pathology is separated into seven pathophysiologic categories:", "</p>",
                                                        "<p style='font-size: 16px'>", "<i>", "(1) Maternal uteroplacental-vascular disorders (e.g. maternal vascular malperfusion)", "</i>", "</p>",
                                                        "<p style='font-size: 16px'>", "<i>", "(2) Fetal stromal-vascular disorders (e.g. fetal vascular malperfusion)", "</i>", "</p>",
                                                        "<p style='font-size: 16px'>", "<i>", "(3) Inflammatory processes (e.g. acute chorioamnionitis)", "</i>", "</p>",
                                                        "<p style='font-size: 16px'>", "<i>", "(4) Malformations/ deformations (e.g. membranous insertion of umbilical cord)", "</i>", "</p>",
                                                        "<p style='font-size: 16px'>", "<i>", "(5) Disruptions (e.g. amniotic band syndrome)", "</i>", "</p>",
                                                        "<p style='font-size: 16px'>", "<i>", "(6) Genomic disorders (e.g. Beckwith-Wiedmann syndrome)", "</i>", "</p>",
                                                        "<p style='font-size: 16px'>", "<i>", "(7) Epigenomic disorders (provisional category without any currently described lesions)", "</i>", "</p>",
                                                        
                                                        "<p style='font-size: 16px'>", "Categories 1 and 2 describe abnormalities related to the maternal and fetal perfusion and are each separated into three subcategories: maldevelopment, malperfusion, and loss of vascular integrity.  Category 3 describes patterns of placental inflammation elicited by infectious organisms, innate inflammatory activators, autoantigens, and maternal or fetal alloantigens.  Categories 4 and 5, malformations/ deformations and disruptions, use the same nomenclature used to classify fetal congenital anomalies to organize disorders of placental structure caused by abnormalities of early placental development. Categories 6 and 7 describe the phenotypic expressions of specific placental genomic and epigenomic abnormalities, respectively.", "</p>",
                                                        "<p style='font-size: 16px'>", "For a more comprehensive discussion of each lesion, we recommend these resources", "<a href='", "https://link.springer.com/book/10.1007/978-3-319-97214-5?noAccess=true", "' target='_blank''>", "(2,", "</a>", "<a href='", "https://assets.cambridge.org/97813166/32536/frontmatter/9781316632536_frontmatter.pdf", "' target='_blank''>", " 3,", "</a>", "<a href='", "https://link.springer.com/book/10.1007/978-3-642-23941-0", "' target='_blank''>", " 4).", "</a>", "</p>",
                                                        
                                                        "<p>", " ", "</p>",
                                                        
                                                        actionLink("link_to_nextpanel", "Click here to get started!", style='color: green; font-size: 18px'),
                                                        
                                                        "<p style='font-size: 16px; colour: white'>",  " ", "</p>",
                                                        "<p style='font-size: 16px; colour: white'>",  " ", "</p>",
                                                        headerPanel(""),
                                                        
                                                        "<p style='font-size: 16px'>", "<b>", "References:", "</b>","</p>",
                                                        
                                                        "<b>"," ","</b>", "<a href='", "https://pubmed.ncbi.nlm.nih.gov/27223167/", "' target='_blank''>", "1. Khong TY, Mooney EE, Ariel I, Balmus NC, Boyd TK, Brundler MA, et al. Sampling and Definitions of Placental Lesions: Amsterdam Placental Workshop Group Consensus Statement. Arch Pathol Lab Med. 2016;140(7):698-713.", "</a>", "<br/>",
                                                        "<b>"," ","</b>", "<a href='", "https://link.springer.com/book/10.1007/978-3-319-97214-5?noAccess=true", "' target='_blank''>", "2. Khong TY, Mooney EE, Nikkels PGJ, Morgan TK, Gordjin SJ, editors. Pathology of the placenta. A practical guide. Switzerland: Springer Nature; 2018.", "</a>", "<br/>",
                                                        "<b>"," ","</b>", "<a href='", "https://assets.cambridge.org/97813166/32536/frontmatter/9781316632536_frontmatter.pdf", "' target='_blank''>", "3. Redline RW, Boyd TK, Roberts DJ, editors. Placental and Gestational Pathology: Cambridge University Press; 2018.", "</a>", "<br/>",
                                                        "<b>"," ","</b>", "<a href='", "https://link.springer.com/book/10.1007/978-3-642-23941-0", "' target='_blank''>", "4. Baergen RN, Burton GJ, Kaplan CG, editors.  Benirschke’s Pathology of the Human Placenta, 2021, Springer Nature; 2021", "</a>", "<br/>"
                                                      )),
                                                      headerPanel(""),
                                                      
                                               )
                                               
                                     )
                            ),
                            
                            tabPanel("Getting started",
                                     
                                     titlePanel(title=h2("Navigating the Placenta Atlas", style = "font-size: 30px")),
                                     
                                     (HTML("<b>Video Tutorial:</b>")),
                                     uiOutput("video"),
                           
                                     
                                     actionButton("UserGuide", "View User Guide", style=" color: black; background-color: white; border-color: black"),
 
                            ),
                            
                            #second tab panel
                            tabPanel("Classification Tree",
                                     shinyjs::useShinyjs(),
                                     #mainPanel(
                                     fluidRow(
                                       column(width= 6,
                                              h3("Classification of placental lesions ", bsButton("q1", label = "", icon = icon("question"),
                                                                                                  style = "info", size = "extra-small"),
                                              ),
                                              #added help text and scrollable box around tree
                                              helpText("Click on each category to explore the various diagnoses and lesions or use the search bar to locate a specific term."),
                                              bsPopover(id="q1", title = "Reminder:", 
                                                        content = paste0(p("1, 2, 3, … = pathophysiologic categories"),
                                                                         p("A, B, C, … = subcategories"),
                                                                         p("I, ii, iii, … = diagnostic headers/ unifying concepts"), 
                                                                         p("a, b, c, … = individual lesions/ specific microscopic findings ")), 
                                                        trigger = "focus", 
                                                        placement = "right",
                                                        options = list(container = "body")),
                                              
                                              box(width = 6, solidHeader = TRUE, 
                                                  (div(style= "width:700px;overflow-x: scroll;height:400px;overflow-y: scroll",
                                                       useShinyjs(),
                                                       actionButton("openAll", "Expand Tree", style=" color: black; background-color: white; border-color: black"),
                                                       
                                                       shinyTree("tree",search= TRUE, theme = "proton")))),
                                       ),   
                                       
                                       column(width = 6,
                                              #may 18th - made the hover info inline = T, this allows the box to appear even after removing mouse from category
                                              # uiOutput("collapseIMAGE", inline=T),
                                              uiOutput("hoverinfo1", inline=T),
                                              uiOutput("wordOneButton"),
                                              
                                              
                                       ),
                                       
                                       #blank space  
                                       headerPanel(""),
                                       headerPanel(""),
                                       
                                       #additional information box  
                                       column(12, 
                                              #hover info - edit may 24
                                              box(width = 12,(div(style= "font-family: 'Arial';
                                            font-size: 10; border-radius: 25px; border: 2px solid #000000; padding: 20px; background-color: #ECECEC", 
                                                                  htmlOutput("hoverinfo"))))
                                       ),
                                       
                                       #downloadable file formats   
                                       tags$head(
                                         tags$style(HTML(".myl {color:blue}")),
                                         #input HTML here - may13th
                                         tags$script(js)
                                       ),
                                       useShinyjs(),
                                       column(width= 12,
                                              titlePanel(title=h3("Downloadable formats", style = "font-size: 25px"),
                                              ),
                                       ),
                                       column(width= 12,       
                                              h2(code("treeToJSON(.)")),
                                              verbatimTextOutput("input_tree"),
                                              helpText("This shinyTree control receives this ", code("data.tree"), " as input.",
                                                       "In order to use that, ", code("treeToJSON(.)"), " is called with the tree.",
                                                       "The resulting JSON can be downloaded below:"),
                                              #downloadbutton
                                              downloadButton("downloadData", "Download JSON"),
                                              #verbatimTextOutput("json")
                                       ),
                                       column(width= 12,       
                                              h2(code("input$tree")),
                                              helpText("Make sure the pathophysiologic category tree has been explored in order to",
                                                       "force rendering of", code("input$tree"), " The resulting tree can be downloaded below:"),
                                              #download the tree displayed from the input$tree button
                                              downloadButton("downloadData1", "Download Tree"),
                                              #verbatimTextOutput("output_tree") # not displaying the downloadable json and tree formats
                                              
                                       ),
                                       #blank space  
                                       headerPanel(""),
                                       headerPanel(""),
                                       headerPanel(""),
                                     )
                            ),
                            
                            tabPanel("Can't find a term?",
                                     titlePanel(title=h2("Search for a term to determine it's associated preferred term", style = "font-size: 30px")),
                                     DT::dataTableOutput("combined")
                                     
                            ),
                            
                            #third tab panel
                            tabPanel("Contact",
                                     p(strong('Contact Details'), style = "font-size: 20px"),
                                     p('Cox Systems Biology lab', style = "font-size: 15px"),
                                     p('Medical Sciences Building', style = "font-size: 15px"),
                                     p('Department of Physiology', style = "font-size: 15px"),
                                     p('Faculty of Medicine', style = "font-size: 15px"),
                                     p('University of Toronto', style = "font-size: 15px"),
                                     a(actionButton(inputId = "email1", 
                                                    label = "email: b.cox@utoronto.ca"),
                                       href='mailto:b.cox@utoronto.ca')
                            )
                 ))




server <- function(input, output, session, x) {
  
  #video output
  output$video <- renderUI({
    tags$video(src = "placenta_tut.mp4", type = "video/mp4", height="25%", width="50%", autoplay = NA, controls = NA)
  })
  
  
  observeEvent(input$UserGuide, {
    showModal(modalDialog(
      title = "User Guide",
      HTML(paste("<p>", "<b>","Step 1:", "</b>", "</p>")),
      HTML(paste("Select the 'Classification Tree' Tab")),
      HTML(paste("<img src='step1.png'width='870'>")),
      HTML(paste("<p>", "<b>","Step 2:", "</b>", "</p>")),
      HTML(paste("Click on the expand button to view all terms")),
      HTML(paste("<img src='step2.png'width='870'>")),
      HTML(paste("<p>", "<b>","Step 3:", "</b>", "</p>")),
      HTML(paste("Use the scroll bar to view all terms")),
      HTML(paste("<img src='step3.png'width='870'>")),
      HTML(paste("<p>", "<b>","Step 4a:", "</b>", "</p>")),
      HTML(paste("Alternatively, enter a specific term into the search field for a more focused search")),
      HTML(paste("<img src='step4a.png'width='870'>")),
      HTML(paste("<p>", "<b>","Step 4b:", "</b>", "</p>")),
      HTML(paste("If you can't locate a specific term within the classification tree, go to the 'Can't find a term' tab")),
      HTML(paste("<img src='step4b.png'width='870'>")),
      HTML(paste("<p>", "<b>","Step 4c:", "</b>", "</p>")),
      HTML(paste("Here you will find a table that contains preferred terms, synonyms, keywords-clinical associations, and keywords-pathophysiology. Enter the specifc term into the table's search bar. Once the term has been identified, copy the corresponding 'preferred term'")),
      HTML(paste("<img src='step4c.png'width='870'>")),
      HTML(paste("<p>", "<b>","Step 4d:", "</b>", "</p>")),
      HTML(paste("Paste or type the 'preferred term' into the classfication tree search bar.")),
      HTML(paste("<img src='step4d.png'width='870'>")),
      HTML(paste("<p>", "<b>","Step 5:", "</b>", "</p>")),
      HTML(paste("Click on the term to view additional information and the corresponding image(s)")),
      HTML(paste("<img src='step5.png'width='870'>")),
      HTML(paste("<p>", "<b>","Step 6:", "</b>", "</p>")),
      HTML(paste("The additional information box contains explanatory text such as synonyms/ alternative terms, definition, current understanding, keywords-clinical associations, keywords-pathophysiology, and references")),
      HTML(paste("<img src='step6.png'width='870'>")),
      HTML(paste("<p>", "<b>","Step 7:", "</b>", "</p>")),
      HTML(paste("Should the term have multiple images, click on the arrow to view all")),
      HTML(paste("<img src='step7.png'width='870'>")),
      HTML(paste("<p>", "<b>","Step 8:", "</b>", "</p>")),
      HTML(paste("Click the button to enlarge the image(s)")),
      HTML(paste("<img src='step8a.png'width='870'>")),
      HTML(paste("<img src='step8b.png'width='870'>")),
      HTML(paste("<p>", "<b>","Step 9:", "</b>", "</p>")),
      HTML(paste("Click the download button to save a copy of the pathology classification tree in JSON or R markdown format")),
      HTML(paste("<img src='step9.png'width='870'>")),
      size="l",
      fade=F,
      easyClose = TRUE))
  })
    
  
  
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })
  
  
  #to expand the shiny tree
  observeEvent(input$openAll, {
    runjs(HTML('$("#tree").jstree("open_all");'))
  })
  
  #to go to next panel
  observeEvent(input$link_to_nextpanel, {
    updateNavbarPage(session, "navbarID",  "Getting started")
  })
  
  print_tree <- function(tree) {
    if (is(tree, "Node")) {
      do.call(print, c(x = tree, as.list(tree$attributesAll)))
    } else {
      str(tree)
    }
  }
  
  get_json <- reactive({
    treeToJSON(osNode, pretty = TRUE)
  })
  
  #output$input_tree  <- renderPrint(print_tree(osNode))
  output$output_tree <- renderPrint(print_tree(req(input$tree)))
  output$json        <- renderPrint(cat(get_json()))   
  output$tree        <- renderTree(get_json())
  
  #download button for json file
  output$downloadData <- downloadHandler(
    filename = function() {
      "get_json.json"
    },
    content = function(file) {
      write_json(get_json(), file)
    }
  )
  
  #download button for the tree
  output$downloadData1 <- downloadHandler(
    filename = function() {
      "input$list.rmd"
    },
    content = function(file) {
      write_yaml(print_tree(osNode), file)
    }
  )
  
  #hover info - added may 13th
  output$hoverinfo <- renderText({
    req(input$hoverednode) 
    
    #convert the name of the category to an object
    x <- as.character(input$hoverednode)
    # retrieve description of the category from my_data df and create an object
    retrievedata <- as.data.frame(my_data[[x]])
    #preferedTerm <- as.character(retrievedata[1,1])
    synonyms <- as.character(retrievedata[2,1])
    definition <- as.character(retrievedata[3,1])
    current_understanding <- as.character(retrievedata[4,1])
    key_clinical <- as.character(retrievedata[5,1])
    key_patho <- as.character(retrievedata[6,1])
    
    
    ref1 <- as.character(retrievedata[7,1])
    link1 <- as.character(retrievedata[8,1])
    ref2 <- as.character(retrievedata[9,1])
    link2 <- as.character(retrievedata[10,1])
    ref3 <- as.character(retrievedata[11,1])
    link3 <- as.character(retrievedata[12,1])
    ref4 <- as.character(retrievedata[13,1])
    link4 <- as.character(retrievedata[14,1])
    
    if (x == "Pathophysiologic category") {
      print(' ')
    } else {
      #this will be the output of the hover text - will appear at the bottom of the tree
      
      HTML(paste("<p>", "<b>","Additional Information: ","</b>", x, "</p>",
                 "<b>","Synonyms/ alternative terms: ","</b>", synonyms,"</p>",
                 "<b>","Definition: ","</b>", definition,"<p/>",
                 "<b>","Current understanding: ","</b>", current_understanding,"</p>",
                 "<b>","Keywords-clinical associations: ","</b>", key_clinical,"</p>",
                 "<b>","Keywords-pathophysiology: ","</b>", key_patho,"</p>",
                 "<b>","References: ","</b>", " ", "</br>",
                 
                 #ADD REFERENCES HERE - M24. need to figure out how to add multiple references
                 
                 "<b>"," ","</b>", "<a href='", link1, "' target='_blank''>", ref1, "</a>", "<br/>",
                 "<b>"," ","</b>", "<a href='", link2, "' target='_blank''>", ref2, "</a>", "<br/>",
                 "<b>"," ","</b>", "<a href='", link3, "' target='_blank''>", ref3, "</a>", "<br/>",
                 "<b>"," ","</b>", "<a href='", link4, "' target='_blank''>", ref4, "</a>"
                 
      ))
    }
  })
  
  #Data Table 
  output$combined <- DT::renderDataTable({
    DT::datatable(lookup_table, options = list(searchHighlight = TRUE, search = list(search = '')))
  })
  
  
  #for image popup
  
  output$hoverinfo1 <- renderUI({
    
    req(input$hoverednode) 
    
    x <- as.character(input$hoverednode)
    
    # create a vector containing letters a-z
    alpha <- letters[1:26]
    # add ".png" to the end of each letter
    alpha1 <- as.character(paste0(alpha, ".png"))
    # add x/the hovered term to "_x.png"
    imagesClick <- as.character(paste(x, alpha1, sep="_"))
    
    # create an empty vector
    my_vec <- character()
    
    # if the name of the term that has been hovered matches one in the files folder then it will be added to my_vec 
    for (r in imagesClick){
      for (m in files) {
        if (r == m) {
          test <- print(m)
          my_vec <- c(my_vec, test)
        }
      }
    }
    
    # count the number of characters in my_vec
    num_image <- length(my_vec)
    
    # for no images
    if (num_image == 0) {
      img(src = "No_image.png", height="60%", width="60%")
      #for one image
    } else if (num_image == 1) {
      img(src = my_vec[1], height="100%", width="100%")
      #for 2 images 
    } else if (num_image == 2) {
      bs_carousel(id = "testing", use_indicators = TRUE) %>%
        bs_append(
          #you need to make another folder in the folder that contains the app. new folder should be called "www". insert image into www folder. 
          content = bs_carousel_image(src = my_vec[1], height="100%", width="100%") ) %>%
        bs_append(
          content = bs_carousel_image(src = my_vec[2], height="100%", width="100%"))
      #for 3 images
    } else if (num_image == 3) {
      bs_carousel(id = "testing", use_indicators = TRUE) %>%
        bs_append(
          #you need to make another folder in the folder that contains the app. new folder should be called "www". insert image into www folder. 
          content = bs_carousel_image(src = my_vec[1], height="100%", width="100%")) %>%
        bs_append(
          content = bs_carousel_image(src = my_vec[2], height="100%", width="100%")) %>%
        bs_append(
          content = bs_carousel_image(src = my_vec[3], height="100%", width="100%"))
      # for 5 images
    } else if (num_image == 5) {
      bs_carousel(id = "testing", use_indicators = TRUE) %>%
        bs_append(
          #you need to make another folder in the folder that contains the app. new folder should be called "www". insert image into www folder. 
          content = bs_carousel_image(src = my_vec[1], height="100%", width="100%")  ) %>%
        bs_append(
          content = bs_carousel_image(src = my_vec[2], height="100%", width="100%")   ) %>%
        bs_append(
          content = bs_carousel_image(src = my_vec[3], height="100%", width="100%") ) %>%
        bs_append(
          content = bs_carousel_image(src = my_vec[4], height="100%", width="100%") ) %>%
        bs_append(
          content = bs_carousel_image(src = my_vec[5], height="100%", width="100%"))
    } else {
      print(' ')
    }
    
  })
  
  #For zoom button - appears conditionally
  output$wordOneButton <- renderUI({
    req(input$hoverednode) 
    
    x <- as.character(input$hoverednode)
    
    # create a vector containing letters a-z
    alpha <- letters[1:26]
    # add ".png" to the end of each letter
    alpha1 <- as.character(paste0(alpha, ".png"))
    # add x/the hovered term to "_x.png"
    imagesClick <- as.character(paste(x, alpha1, sep="_"))
    
    # create an empty vector
    my_vec <- character()
    
    # if the name of the term that has been hovered matches one in the files folder then it will be added to my_vec 
    for (r in imagesClick){
      for (m in files) {
        if (r == m) {
          test <- print(m)
          my_vec <- c(my_vec, test)
        }
      }
    }
    
    # count the number of characters in my_vec
    num_image <- length(my_vec)
    
    
    if (num_image == 0) {
      print(' ')
      #for one image
    } else if (as.character(input$hoverednode) == "Pathophysiologic category"){
      print(' ')
    } else if (as.character(input$hoverednode) == "1_Maternal_uteroplacental_vascular_disorders"){
      print(' ')
    } else if (as.character(input$hoverednode) == "2_Fetal_stromal-vascular_disorders"){
      print(' ')
    } else if (as.character(input$hoverednode) == "3_Inflammatory_processes"){
      print(' ')
    } else if (as.character(input$hoverednode) == "4_Malformation-deformation"){
      print(' ')
    } else if (as.character(input$hoverednode) == "5_Disruptions"){
      print(' ')
    } else if (as.character(input$hoverednode) == "6_Genomic_disorders"){
      print(' ')
    } else if (as.character(input$hoverednode) == "7_Epigenomic_disorders"){
      print(' ')
      # if input$hoverednode is NOT equal to any of the above terms THEN the action button will appear
    } else {
      actionButton("action", "Click to enlarge!", style=" color: black; background-color: white; border-color: black")
    }
  })
  
  #for images to zoom
  observeEvent(input$action, {
    
    req(input$hoverednode) 
    x <- as.character(input$hoverednode)
    
    
    # create a vector containing letters a-z
    alpha <- letters[1:26]
    # add ".png" to the end of each letter
    alpha1 <- as.character(paste0(alpha, ".png"))
    # add x/the hovered term to "_x.png"
    imagesClick <- as.character(paste(x, alpha1, sep="_"))
    
    # create an empty vector
    my_vec <- character()
    
    # if the name of the term that has been hovered matches one in the files folder then it will be added to my_vec 
    for (r in imagesClick){
      for (m in files) {
        if (r == m) {
          test <- print(m)
          my_vec <- c(my_vec, test)
        }
      }
    }
    
    # count the number of characters in my_vec
    num_image <- length(my_vec)
    
    # for no images
    if (num_image == 0) {
      print(' ')
      #for one image
    } else if (num_image == 1){
      showModal(modalDialog(
        title = "Here is the selected image!",
        HTML(paste("<img src='",my_vec[1],"'width='870'>")),
        size="l",
        fade=F,
        easyClose = TRUE))
      #for 2 images 
    } else if (num_image == 2){
      showModal(modalDialog(
        title = "Here are the selected images!",
        HTML(paste("Image 1")),
        HTML(paste("<img src='",my_vec[1],"'width='870'>")),
        HTML(paste("Image 2")),
        HTML(paste("<img src='",my_vec[2],"'width='870'>")),
        size="l",
        fade=F,
        easyClose = TRUE))
      #for three images
    }else if (num_image == 3){
      showModal(modalDialog(
        title = "Here are the selected images!",
        HTML(paste("Image 1")),
        HTML(paste("<img src='",my_vec[1],"'width='870'>")),
        HTML(paste("Image 2")),
        HTML(paste("<img src='",my_vec[2],"'width='870'>")),
        HTML(paste("Image 3")),
        HTML(paste("<img src='",my_vec[3],"'width='870'>")),
        size="l",
        fade=F,
        easyClose = TRUE))
      #for five images
    } else if (num_image == 5){
      showModal(modalDialog(
        title = "Here are the selected images!",
        HTML(paste("Image 1")),
        HTML(paste("<img src='",my_vec[1],"'width='870'>")),
        HTML(paste("Image 2")),
        HTML(paste("<img src='",my_vec[2],"'width='870'>")),
        HTML(paste("Image 3")),
        HTML(paste("<img src='",my_vec[3],"'width='870'>")),
        HTML(paste("Image 4")),
        HTML(paste("<img src='",my_vec[4],"'width='870'>")),
        HTML(paste("Image 5")),
        HTML(paste("<img src='",my_vec[5],"'width='870'>")),
        size="l",
        fade=F,
        easyClose = TRUE))
      #for anything else 
    } else {
      print(' ')
    }
    
    
  })
  
  #end of for image popup
}

shinyApp(ui, server)
