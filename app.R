library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(shinycssloaders)
library(kableExtra)
library(shinyjs)
library(COGugaison)
library(cgwtools)

# Préparation des COG
list_data <- data(package = "COGugaison")$results[, "Item"]
list_an_COG <- list_data[substr(list_data, 1, 3) == "COG"] %>%
  substr(., 4, 7) %>%
  unique() %>%
  as.numeric() %>%
  sort(decreasing = T)

options(shiny.maxRequestSize = 100*1024^2)

# Define UI for data upload app ----
ui <- fluidPage(
    
    useShinyjs(),
    
    # Lien css
    includeCSS("style.css"),
    tags$head(
        tags$style(HTML("
              @import url('//fonts.googleapis.com/css?family=Bree+Serif');
              @import url('//fonts.googleapis.com/css?family=Ubuntu');
              @import url('https://use.fontawesome.com/releases/v5.7.2/css/all.css');
            ")),
        tags$title(HTML("diaCOG - Diagnostic de COG")),
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        tags$style("tfoot {display: table-header-group;}")
    ),
    
    navbarPage(title = div(id = "bloc-titre",
                           img(src = 'logo_v3.png', id = 'logo')),
               id = "diacog-navbar",
               tabPanel("Diagnostic",
                        # Sidebar layout with input and output definitions ----
                        sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            sidebarPanel(id ="panel-left",
                                         
                                         
                                         # Input : sélectionner un fichier
                                         fileInput("file1", "Sélectionner un fichier",
                                                   multiple = FALSE,
                                                   buttonLabel = "Parcourir",
                                                   placeholder = "Aucun fichier sélectionné"),
                                         
                                         # Précisions sur le format
                                         tags$div(class = "nb",
                                                  tags$p("Formats acceptés : csv, xlsx et xls"),
                                                  tags$p("Taille maximale : 100MB"),
                                                  tags$p("Attention : le temps de chargement est proportionnel à la taille du fichier."),
                                                  tags$p("Si votre fichier est au format csv et que certains codes communes commencent par des 0, assurez-vous que l'enregistrement en tient bien compte ou, changez de format.")
                                         ),
                                         
                                         # Input : choix de la variable à cogidentifier
                                         selectizeInput(
                                           inputId = "codgeo_init",
                                           label = "Code communal à diagnostiquer",
                                           choices = NULL,
                                           multiple = FALSE,
                                           selected = NULL),
                                         
                                         # Options supplémentaires
                                         h4("Options de diagnostic"),
                                         # 1. Ignorer les codes manquants
                                         checkboxInput(
                                             inputId = "ign_na",
                                             label = "Ignorer les codes manquants"
                                         ),
                                         # 2. Ajouter une colonne de doublons à la table en sortie
                                         checkboxInput(
                                             inputId = "col_doub",
                                             label = "Ajouter une colonne d'identification des codes en double"
                                         ),
                                         
                                         # Hypothèse de COG (proposition d'ajout KA)
                                         selectizeInput(
                                           inputId = "hypothese_COG",
                                           label = "Hypothèse de COG (paramètre avancé)",
                                           choices = list_an_COG,
                                           multiple = FALSE,
                                           selected = "2021"
                                         ),
                                         
                                         # Précisions sur l'hypothèse de COG
                                         tags$div(class = "nb",
                                                  tags$p("Le paramètre d'hypothèse de COG permet de comparer le fichier en entrée avec un COG de référence différent du COG de référence le plus récent. Le fichier est alors comparé prioritairement à ce millésime de COG puis aux millésimes de COG les plus proches dans le temps."),
                                                  tags$br()
                                         ),
                                         
                                         # Bouton de lancement du diagnostic
                                         actionButton(
                                             inputId = "start_diag",
                                             label = "Lancer le diagnostic"
                                         )
                            ),
                            
                            # Main panel for displaying outputs ----
                            mainPanel(
                                
                                # Tabsetpanel
                                column(10,
                                       fluidRow(
                                           tabsetPanel(type = "tabs",
                                                       tabPanel("Diagnostic",
                                                                withSpinner(
                                                                    uiOutput("diag_COG_txt"),
                                                                    type = 8,
                                                                    color = '#d43f00'),
                                                                tableOutput("diag_tab")),
                                                       tabPanel("Tableau",
                                                                br(),
                                                                downloadButton('download',"Télécharger les données"),
                                                                br(),
                                                                dataTableOutput("table_exp")))                                           
                                       ))
                            )
                        )
               ),
               tabPanel("À propos",
                        column(1),
                        column(8,
                               fluidRow(
                                   span(class = "highlight", "À propos"),
                                   br(),
                                   h3("Qu’est-ce qu’un diagnostic de COG ?"),
                                   p(class = "apropos-txt", "L’acronyme « COG » désigne le « code officiel géographique ». Il s’agit de l’ensemble des codes Insee associés à chaque territoire administratif (les communes en l’occurrence) pour une année donnée. Chaque année, un certain nombre de communes changent de périmètre (par fusion ou défusion) et, comme pour les vins, les codes communes ont ainsi un « millésime ». Le diagnostic de COG permet d’identifier ce millésime ou, si ça n’est pas possible, d’identifier les codes présents dans la base qui empêchent ce diagnostic."),
                                   h3("Pourquoi et quand faire un diagnostic de COG ?"),
                                   p(class = "apropos-txt", "Quand on souhaite manipuler un tableau de données à l’échelon communal mais qu’on ne sait pas à quel millésime de COG il correspond ou quand on a des doutes sur la qualité du référentiel communal (dans le cas de données saisies manuellement par exemple), un diagnostic de COG s’impose. Celui-ci est indispensable pour effectuer des croisements de données par jointure ou en préalable au nettoyage d’un référentiel communal en vue de traitements futurs."),
                                   h3("Qu’est-ce qu’un référentiel communal cohérent ?"),
                                   p(class = "apropos-txt", "Le référentiel communal désigne l’ensemble des codes contenus dans une base de données et servant à identifier des communes. On estime qu’un référentiel communal est cohérent quand l’ensemble des codes communes qui le composent sont présents dans un même millésime de COG."),
                                   h3("Quelles sont les étapes du diagnostic de COG ?"),
                                   p(class = "apropos-txt", "Suivant le degré de « propreté » du référentiel communal, le diagnostic de COG se déroule en 3 étapes : "),
                                   tags$ul(class = "apropos-txt", 
                                       tags$li("on commence par identifier les codes qui correspondent à des codes de collectivités d’outre-mer ou à des codes d’arrondissements municipaux ; ceux-ci sont identifiés et mis de côté pour la seconde étape du diagnostic ;"),
                                       tags$li("pour chaque millésime de COG, de 2021 à 1968, on vérifie ensuite si l’ensemble des codes communes du référentiel communal à diagnostiquer sont présents. Si c’est le cas, le diagnostic renvoie le COG le plus récent contenant l’ensemble des communes du référentiel communal à diagnostiquer ;"),
                                       tags$li("si aucun millésime de COG ne contient l’ensemble des codes présents dans le référentiel communal à diagnostiquer, alors le COG de référence est jugé « non identifiable » et on cherche, pour chaque code, le millésime de COG le plus récent où ce code a existé ou, s’il n’a jamais existé (par exemple un code « ZZZZZ »), on identifie ce code comme « indéterminé ». ")
                                  ),
                                  h3("À quoi sert le paramètre d'hypothèse de COG ?"),
                                  p(class = "apropos-txt", "Le paramètre d'hypothèse de COG permet de modifier l'ordre de comparaison entre le fichier en entrée et chaque millésime de COG. Ce paramètre permet ainsi d'intervenir sur la seconde étape du diagnostic de COG (cf. question précédente). Dans le cas où le référentiel communal est cohérent, l'algorithme de détection de COG regarde en priorité le millésime de COG donné en hypothèse, puis passe en revue l'ensemble des millésimes de COG par ordre de proximité temporel. Le diagnostic de COG correspond alors au millésime de COG le plus proche du millésime donné en hypothèse, contenant l’ensemble des communes du référentiel communal à diagnostiquer. Dans le cas contraire (COG non identifiable), l'algorithme renvoie, pour chaque code commune et quand c’est possible, le millésime de COG le plus proche dans le temps du COG donné en hypothèse où ce code apparaît."),
                                  h3("Que faire si le diagnostic de COG renvoie « COG non identifiable » ?"),
                                  p(class = "apropos-txt", "Le COG est jugé « non identifiable » dans 3 cas :"),
                                  tags$ul(class = "apropos-txt", 
                                      tags$li("si le référentiel communal contient des valeurs manquantes et vous n'avez pas coché la case « Ignorer les codes manquants »,"),
                                      tags$li("si le référentiel communal est, de façon évidente, historiquement incohérent, c’est-à-dire qu’il mélange des codes issus de millésimes de COG différents,"),
                                      tags$li("si le référentiel communal contient au minimum 1 code « indéterminé », c’est-à-dire qui n’est présent dans aucun millésime de COG depuis 1968.")
                                  ),
                                  p(class = "apropos-txt", "Dans le premier cas, on peut cocher la case « Ignorer les codes manquants » avant de relancer le diagnostic."),
                                  p(class = "apropos-txt", "Dans le second cas, suivant la nature des données on peut envisager de convertir le référentiel communal dans le COG le plus récent présent dans le base en recherchant, pour chaque commune de COG antérieur au COG le plus récent présent dans la base, son code actuel (ce travail peut être fait de façon automatisée grâce au package R", a(href = "https://antuki.github.io/COGugaison/index.html", "COGugaison")," )."),
                                  p(class = "apropos-txt", "Dans le troisième cas, on peut corriger manuellement les codes communes à partir d’un champ de libellé par exemple ou, si ça n’est pas possible, supprimer les observations de la base pour lesquelles le code commune est indéterminable avant de relancer un diagnostic."),
                                  p(class = "apropos-txt", "Ces 3 opérations de « nettoyage » comprennent un risque d’erreur mais sont indispensables à un traitement rigoureux des données."),
                                  h3("Quelles sont les limites du diagnostic de COG ? "),
                                  p(class = "apropos-txt", "Le diagnostic de COG repose sur l’hypothèse que le référentiel communal présent dans un fichier est historiquement cohérent, c’est-à-dire que l’ensemble des codes communes qui le composent sont rattachés à un seul et même COG. Cette hypothèse n’est pas toujours vraie mais il est presque impossible de l’invalider, sauf dans le cas, rare, où le référentiel contient des codes de communes qui n’ont pas existé au même moment (cf. question suivante). Quand il est possible, le diagnostic de COG renvoie alors le millésime de COG le plus récent contenant l’ensemble des codes communes du référentiel communal diagnostiqué ou, si le paramètre d’hypothèse de COG est différent, le COG le plus proche d’un point de vue temporel du « millésime-hypothèse »."),
                                  h3("Qu’est-ce qu’un référentiel communal historiquement incohérent ?"),
                                  p(class = "apropos-txt", "Parfois, il arrive qu’un référentiel communal soit historiquement incohérent, c’est-à-dire qu’il contienne, de façon évidente, des codes Insee issus de COG différents. C’est par exemple le cas de bases de données constituées « au fil de l’eau » sur plusieurs années (répertoire d’entreprises, zonage règlementaire…)."),
                                  h3("Qui développe diaCOG ?"),
                                  p(class = "apropos-txt", "diaCOG est développé par l’Observatoire des territoires de l’ANCT."),
                                  h3("Est-il possible de faire un diagnostic de COG directement depuis R ?"),
                                  p(class = "apropos-txt", "Absolument ! Cette fonction a été implémentée dans le package R, développé par Kim Antunez (Insee). La fonction diag_COG() renvoie un résultat similaire directement dans la console de R Studio. La ",tags$a(href = "https://antuki.github.io/COGugaison/articles/COGugaison.html#quel-code-officiel-g-ographique-est-utilis-", "vignette du package ",),"permet de faire le lien entre cette fonction et les autres fonctions du package."),
                                  h3("Comment obtenir de l’aide sur diaCOG ?"),
                                  p(class = "apropos-txt", "Vous pouvez contacter l'équipe de l'Observatoire des territoires à l'adresse mail : observatoire@anct.gouv.fr ou via ", tags$a(href = "https://www.observatoire-des-territoires.gouv.fr/form/nous-contacter", "notre plateforme de contact.")),
                                  br(),
                                  a(href = "https://www.observatoire-des-territoires.gouv.fr/node/27", img(src = "logo_OT.png", class = "apropos-logos")),
                                  a(href = 'https://agence-cohesion-territoires.gouv.fr/', img(src = "ANCT_Logo.png", class = "apropos-logos")),
                                  a(href = 'https://antuki.github.io/COGugaison/index.html', img(src = "logo_package.png", class = "apropos-logos"))
                               )
                               ))
   )
                                  
)

# Define server logic to read selected file ----
server <- function(session, input, output) {
    
    # Reactive values
    values <- reactiveValues(
        curr_df = 0,
        curr_cog = 0,
        tab_exp = NULL,
        ready = F
        )

    # Fonctions
    # Lecture du fichier
    observeEvent(input$file1, {
        
        path <- input$file1$datapath
        ext <- gsub(".*\\.", "", path)
        
        # Lire le df
        if(ext == "xlsx"){
            values$curr_df <- read_xlsx(path)
        }
        if(ext == "xls"){
            values$curr_df <- read_xls(path)
        }
        if(ext == "csv"){
            values$curr_df <- read.csv2(path, stringsAsFactors = F)
        }
                
        # Adapter contenu menu déroulant
        updateSelectizeInput(session,
                          inputId = "codgeo_init",
                          choices = colnames(values$curr_df),
                          selected = colnames(values$curr_df)[1])
    
    })

    # Calcul du diagnostic
    observeEvent(input$start_diag, {
      
        values$ready <- T
        
        # Script d'identification de COG
        codgeo <- input$codgeo_init
        temp <- values$curr_df %>%
            select(!!as.name(codgeo)) %>%
            setNames("codgeo_init")
        
        # ****************************************************************************
        # Détection d'erreurs de codes
        # Codes manquants
        temp.na <- temp %>%
            filter(is.na(codgeo_init))
        
        # Codes arrondissements
        temp.plm <<- temp %>%
            filter((substr(codgeo_init, 1, 3) %in% c("751", "132") | substr(codgeo_init, 1, 4) == "6938") & nchar(codgeo_init) == 5)
        
        # Communes des collectivités d'outre-mer
        temp.com <- temp %>%
            filter((substr(codgeo_init, 1, 2) == "98" | substr(codgeo_init, 1, 3) %in% c("975", "977", "978", "979")) & nchar(codgeo_init) == 5)
        
        # ****************************************************************************
        # Diagnostic de COG
        diacog.exp <- values$curr_df
        
        if(input$hypothese_COG!=list_an_COG[1]){
          list_an_COG <- list_an_COG[order(abs(as.numeric(input$hypothese_COG)-list_an_COG))]
        }

        # Si on fait une hypothèse sur le COG, on change l'ordre de parcourt des années.
        # On commence par l'hypothèse puis on parcourt les années des plus proches de celle-ci (plus proches voisins)

        # Algorithme de détection de COG
        i <- 1
        cog_propre <- FALSE
        
        if(input$ign_na == F){
          df_to_test <- temp %>%
            filter(!(codgeo_init %in% temp.plm$codgeo_init) & !(codgeo_init %in% temp.com$codgeo_init))
        } else if(input$ign_na == T) {
          df_to_test <- temp %>%
            filter(!is.na(codgeo_init)) %>%
            filter(!(codgeo_init %in% temp.plm$codgeo_init) & !(codgeo_init %in% temp.com$codgeo_init))
        }
        
        while(cog_propre == FALSE){
            an <- list_an_COG[i]
            nom_COG <- paste0("COG", as.character(an))
        
            if(!is.na(an)){
        
                df_COG <- eval(parse(text = nom_COG))

                nb_obs_abs <- nrow(filter(df_to_test, !(codgeo_init %in% df_COG$CODGEO)))
        
                if(nb_obs_abs == 0 & nrow(temp.com) == 0 & nrow(temp.plm) == 0){
                    cog_propre <- TRUE
                    values$curr_cog <- nom_COG
                    # Compléter le fichier d'export (même valeur pour chaque ligne)
                    diacog.exp[["DIAcoG-idcog"]] <- nom_COG
                }
                if(nb_obs_abs == 0 & (nrow(temp.com) > 0 | nrow(temp.plm) > 0)){
                    cog_propre <- TRUE
                    values$curr_cog <- nom_COG
                    # Compléter le fichier d'export
                    diacog.exp <- diacog.exp %>%
                        mutate(`DIAcoG-idcog` = case_when(!!as.name(codgeo) %in% temp.plm$codgeo_init ~ "arrondissement municipal",
                                                          !!as.name(codgeo) %in% temp.com$codgeo_init ~ "collectivité d'outre-mer",
                                                          is.na(!!as.name(codgeo)) ~ "code manquant",
                                                          !!as.name(codgeo) %in% df_COG$CODGEO ~ nom_COG,
                                                          TRUE ~ "code indéterminé"))
                }
                else {
                    i <- i+1
                }
            }
            else{
                cog_propre <- "non identifiable"
                values$curr_cog <- "COG non identifiable"
            }
        }
        
        # Si COG indétectable
        if(cog_propre == "non identifiable"){
          
          list_an_COG <- rev(list_an_COG)
          
          # VERSION 2 : simplification proposée par Kim Antunez pour boucler sur les années plutôt que sur les codes communes
          # Initialisation de la variable de diagnostic
          diacog.exp$`DIAcoG-idcog` <- NA
          
          # On parcourt toutes les années
          for(an in list_an_COG){
            nom_COG <- paste0("COG", as.character(an))
            df_COG <- eval(parse(text = nom_COG))
            
            diacog.exp <- diacog.exp %>%
              mutate(`DIAcoG-idcog` = ifelse(!!as.name(codgeo) %in% df_COG$CODGEO, nom_COG, `DIAcoG-idcog`))

          }
          
          # On traite le cas des codes communes particuliers
          diacog.exp <- diacog.exp %>%
            mutate(`DIAcoG-idcog` = case_when(is.na(!!as.name(codgeo)) ~ "code manquant",
                                              !!as.name(codgeo) %in% temp.com$codgeo_init ~ "collectivité d'outre-mer",
                                              !!as.name(codgeo) %in% temp.plm$codgeo_init ~ "arrondissement municipal",
                                              TRUE ~ `DIAcoG-idcog`))
          
          diacog.exp$`DIAcoG-idcog`[is.na(diacog.exp$`DIAcoG-idcog`)] <- "code indéterminé"
          
            # VERSION 1 : CL
            # for(code in diacog.exp[[codgeo]]){
            #     i <- 1
            #     code_id <- FALSE
            # 
            #     while(code_id == FALSE){
            #         an <- list_an_COG[i]
            #         nom_COG <- paste0("COG", as.character(an))
            # 
            #         if(!is.na(an)){
            #             df_COG <- eval(parse(text = nom_COG))
            #             if(code %in% df_COG[["CODGEO"]]){
            #                 diacog.exp[["DIAcoG-idcog"]][diacog.exp[[codgeo]]==code] <- nom_COG
            #                 code_id <- TRUE
            #             }
            #             else{
            #                 i <- i+1
            #             }
            #         }
            #         else{
            #             if(code %in% temp.com$codgeo_init & code_id == FALSE){
            #                 diacog.exp[["DIAcoG-idcog"]][diacog.exp[[codgeo]]==code] <- "collectivité d'outre-mer"
            #                 code_id <- TRUE
            #             }
            #             if(code %in% temp.plm$codgeo_init & code_id == FALSE){
            #                 diacog.exp[["DIAcoG-idcog"]][diacog.exp[[codgeo]]==code] <- "arrondissement municipal (Paris, Lyon, Marseille)"  
            #                 code_id <- TRUE
            #             }
            #             else if(code_id == FALSE){
            #                 diacog.exp[["DIAcoG-idcog"]][diacog.exp[[codgeo]]==code] <- "code indéterminé" 
            #                 code_id <- TRUE
            #             }
            #         }
            #     }
            # }
        }
        #diacog.exp[["DIAcoG-idcog"]][is.na(diacog.exp[[codgeo]])] <- "code manquant"
        
        if(input$col_doub == F){
            diacog.exp <- diacog.exp %>%
            select(!!as.name(codgeo), `DIAcoG-idcog`, everything())
        }
        if(input$col_doub == T){
            diacog.exp <- diacog.exp %>%
                group_by(!!as.name(codgeo)) %>%
                mutate("DIAcoG-doubl" = case_when(n()>1 ~ "code doublonné",
                                                  TRUE ~ "code unique")) %>%
                ungroup() %>%
                select(!!as.name(codgeo), `DIAcoG-idcog`, `DIAcoG-doubl`, everything())
        }
    
        values$tab_exp <- diacog.exp
    
    })
    
    # Renvoi du diagnostic de COG
    output$diag_COG_txt <- renderUI({
      
        if(values$ready == T){
            
            # Préparation
            obs_ens <- nrow(values$curr_df)
            obs_un <- length(unique(values$tab_exp[[input$codgeo_init]]))
            
            temp <- table(values$tab_exp$`DIAcoG-idcog`) %>%
                as.data.frame() %>%
                setNames(c("mod", "nb_obs")) %>%
                arrange(desc(nb_obs)) %>%
                rbind(data.frame(mod = "codes uniques", nb_obs = obs_un))
            
            if(length(unique(values$tab_exp[[input$codgeo_init]])) != nrow(values$tab_exp)){
              txt_doubl <- "Le fichier comprend des doublons de code commune."
            } else {
              txt_doubl <- ""
            }
            
            # Affichage
            if(values$curr_cog != "COG non identifiable"){
                
                df_cog <- eval(parse(text = values$curr_cog))
                nb_com_tot <- nrow(df_cog)
                
                HTML("
                  <br/>
                  <span class =
                  'highlight'>Synthèse globale</span>
                  <br/>
                  <p class = 'wazne'>",values$curr_cog,"</p>
                  <p class = 'nb'>Hypothèse de COG en entrée : ", input$hypothese_COG, "</p>
                  <p class = 'nb'>Nombre de communes total du millésime de COG : ", nb_com_tot,"</p>
                  <p class = 'nb'>", txt_doubl,"</p>
                  <span class = 'highlight'>Diagnostic détaillé</span>
              <br/>
              <p>Le fichier compte <span class = 'wazne'>", obs_ens, "</span> observations dont :</p>
                 <br/>", kable(temp,col.names = NULL,
                               align = "ll",
                               full_width = F,
                               format.args = list(big.mark = " ",
                                                  scientific = FALSE))%>%
                         column_spec(1, width = "14em") %>%
                         column_spec(2, bold = T),
                     "<br/><br/>
              <p class = 'comm-methode'><i class='fas fa-lightbulb'></i>&nbsp;&nbsp;Le diagnostic de COG correspond au <strong>COG le plus proche de l'hypothèse de COG en entrée (le plus récent par défaut) dans lequel l'ensemble des codes communes du fichier en entrée sont présents</strong>.</p>
                     ")
            } else {
                HTML("
                  <br/>
                  <span class =
                  'highlight'>Synthèse globale</span>
                  <br/>
                  <p class = 'wazne'>",values$curr_cog,"</p>
                  <p class = 'nb'>Hypothèse de COG en entrée : ", input$hypothese_COG, "</p>
                  <p class = 'nb'>", txt_doubl,"</p>
                  <span class = 'highlight'>Diagnostic détaillé</span>
                  <br/>
                  <p>Le fichier compte <span class = 'wazne'>", obs_ens, "</span> observations dont :</p>
                  <br/>", kable(temp,col.names = NULL,
                               align = "ll",
                               full_width = F,
                               format.args = list(big.mark = " ",
                                                  scientific = FALSE))%>%
                         column_spec(1, width = "14em") %>%
                         column_spec(2, bold = T),
                     "<br/><br/>
              <p class = 'comm-methode'><i class='fas fa-lightbulb'></i>&nbsp;&nbsp;Dans la mesure où le COG n'est pas identifiable, le diagnostic de COG correspond au <strong>COG le plus proche de l'hypothèse de COG en entrée (le plus récent par défaut) d'apparition de chaque code</strong> commune du fichier en entrée.</p>
              <br/>
              <p class = 'comm-methode'><i class='fas fa-arrow-circle-right'></i>&nbsp;&nbsp;Rendez-vous sur la page À propos pour savoir quoi faire avec un fichier dont le COG n'est pas identifiable.</p>
                     ")                
            }
        } else {
            HTML("<br/>
                 <p class = 'comm-methode'>Sélectionnez un fichier et un champ pour lancer le diagnostic.</p>")
        }
    })
    
    # Tableau avec colonne de diagnostic de COG
    output$table_exp <- renderDataTable(values$tab_exp,
                                        options = list(pageLength = 15, lengthChange = FALSE,
                                                       dom  = '<"top">lrt<"bottom">ip'))
    
    # Télécharger les données
    output$download <- downloadHandler(
        filename = function(){"diacog-exp.csv"}, 
        content = function(filename){
            write.csv(values$tab_exp, filename, row.names = F)
    })
    
    # Vider le diagnostic si changement de fichier / de paramètres / de colonne sélectionnée
    observeEvent(input$file1, {
        values$ready <- F
    })
    observeEvent(input$codgeo_init, {
      values$ready <- F
    })
    observeEvent(input$ign_na, {
      values$ready <- F
    })
    observeEvent(input$col_doub, {
      values$ready <- F
    })
    observeEvent(input$hypothese_COG, {
      values$ready <- F
    })

}

shinyApp(ui, server)
