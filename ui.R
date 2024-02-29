library(bslib)

ui = navbarPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  "Análise de Eventos Extremos",
  
  tags$head(
    tags$style(HTML("* {font-family: 'Roboto', sans-serif;}"))
    ),
  
  navbarMenu(
    "Sobre",
    
    tabPanel(
      "Instruções",
      
      navlistPanel(
        id = "tabset",
        "1) Sobre",
        tabPanel(
          "1.1) Instruções", 
          card(
            height = 665,
            card_body(
              h1("Bem-vindo(a) ao aplicativo!"),
              
              p('Se você é novo(a) por aqui, este é um aplicativo genérico para 
              análise de eventos extremos.'),
              
              p('Recomendamos que, antes de começar a sua análise, você siga as 
              instruções presentes nessa aba.'),
              
              p('Se você não faz ideia do que são eventos extremos, você pode 
              seguir a ordem dos conteúdos à esquerda (tópico 1.2 ao 7). Agora, 
              se você já sabe do que se tratam esses eventos, você pode ir 
              direto para o tópico 2 e seguir adiante até o tópico 7.'),
              
              p('Por favor, se você encontrar algum problema durante a sua 
              análise ou quiser dar algum feedback sobre o aplicativo, não deixe 
              de nos escrever através do', 
                a(href='https://www.google.com/intl/pt-BR/forms/about/',
                  'formulário'),
                '. Sua opinião é muito importante para que o aplicativo continue 
                melhorando!')
              )
            )
          ),
        
        tabPanel("1.2) Teoria de Valores Extremos", ""),
        tabPanel("1.3) Autores", ""),
        tabPanel("1.4) Agradecimentos", ""),
        tabPanel("1.5) Referências", ""),
        "2) Tratamento",
        tabPanel("Conteúdo", ""),
        "3) Estatísticas",
        tabPanel("Conteúdo", ""),
        "4) Parâmetros",
        tabPanel("Conteúdo", ""),
        "5) Qualidade do Ajuste",
        tabPanel("5.1) Gráficos", ""),
        tabPanel("5.2) Tabela", ""),
        "6) Probabilidades",
        tabPanel("Conteúdo", ""),
        "7) Níveis de Retorno",
        tabPanel("Conteúdo", ""),
        
      )
    ),
    
    tabPanel(
      "Teoria de Valores Extremos"
      ),
    
    tabPanel(
      "Autores"
    ),
    
    tabPanel(
      "Agradecimentos"
    ),
    
    tabPanel(
      "Referências"
    )
  ),
  
  tabPanel(
    "Tratamento",
    
    fluidRow(
      column(3),
      column(6,
             card(
               card_header(
                 strong("Selecione o conjunto de dados:")
                 ),
               card_body( 
                 fileInput(inputId = "input_file",
                           label = NULL,
                           multiple = FALSE, 
                           accept = ".txt",
                           width = 550)
                 )
               )
             ),
      column(3)
      ),
    
    fluidRow(
      column(3),
      column(6,
             card(
               card_header(
                 strong("Selecione o método de extração de extremos:")
                 ),
               card_body(
                 radioButtons(
                   inputId = "input_method",
                   label = NULL,
                   choices = list("Blocos" = 1, "Picos" = 2),
                   selected = 1),
                 
                 uiOutput("output_threshold")
                 )
               )
             ),
      column(3)
      ),
    
    fluidRow(
      column(3),
      column(6,
             card(
               card_header(
                 strong("Digite o último ano da série de treinamento:")
                 ),
               card_body(
                 numericInput(
                   inputId = "input_year", 
                   label = NULL,
                   value = NULL,
                   width = 550)
                 )
               )
             ),
      column(3)
      ),
    
    fluidRow(
      column(3),
      column(6,
             card(
               card_header(
                 strong("Digite o nome da variável a ser analisada:")
                 ),
               card_body(
                 textInput(inputId = "input_name",
                           label = NULL,
                           width = 550)
                 )
               )
             ),
      column(3)
      ),
    
    fluidRow(
      column(3),
      column(6,
             card(
               card_header(
               strong("Digite a unidade de medida da variável a ser analisada:")
               ),
               card_body(
                 textInput(inputId = "input_uom",
                           label = NULL,
                           width = 550)
                 )
               )
             ),
      column(3)
      ),
    
    fluidRow(
      column(3),
      column(6,
             card(
               card_header(
                 strong("Insira, em decimal com ponto, o nível de significância 
                 para os testes de qualidade de ajuste (o padrão é 0.05):")
               ),
               card_body(
                 textInput(inputId = "input_sl",
                           label = NULL,
                           width = 550)
               )
             )
      ),
      column(3)
    ),
    
    fluidRow(
      column(3),
      column(6,
             card(
               card_header(
               strong("Insira até 12 níveis extremos da variável que você deseja 
               calcular probabilidades. Para números decimais use ponto ao 
               invés de vírgula e separe os níveis com ponto e vírgula:")
               ),
               card_body(
                 textInput(
                   inputId = "input_probs",
                   label = NULL,
                   width = 550)
                 )
               )
             ),
      column(3)
      ),
    
    fluidRow(
      column(3),
      column(6,
             card(
               card_header(
               strong("Insira até 12 tempos de retorno, em anos, para os quais 
               você deseja calcular níveis de retorno extremos da variável. Para 
               números decimais use ponto ao invés de vírgula e separe os tempos 
                      com ponto e vírgula:"),
               ),
               card_body(
                 textInput(
                   inputId = "input_rls",
                   label = NULL,
                   width = 550)
                 )
               )
             ),
      column(3)
      )
    ),
  
  tabPanel(
    "Estatísticas", 
    
    DT::dataTableOutput("output_stcs")
  ),
  
  tabPanel(
    "Parâmetros",
    
    DT::dataTableOutput("output_pes")
  ),
  
  navbarMenu(
    "Qualidade de Ajuste",
    
    tabPanel(
      "Gráficos",
    
      plotOutput("output_ann"),
      plotOutput("output_jan"),
      plotOutput("output_feb"),
      plotOutput("output_mar"),
      plotOutput("output_apr"),
      plotOutput("output_may"),
      plotOutput("output_jun"),
      plotOutput("output_jul"),
      plotOutput("output_aug"),
      plotOutput("output_sep"),
      plotOutput("output_oct"),
      plotOutput("output_nov"),
      plotOutput("output_dec")
    ),
    
    tabPanel(
      "Tabela",
      
      DT::dataTableOutput("output_gof")
    )
  ),
  
  tabPanel(
    "Probabilidades", 
    
    DT::dataTableOutput("output_probs")
  ),
  
  tabPanel(
    "Níveis de Retorno", 
    
    DT::dataTableOutput("output_rls")
  )
)