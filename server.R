library(DT)
library(fBasics)
library(ggplot2)

source("functions_server.R")

options(DT.options = 
          list(pageLength = 13,
               dom = 'Bt',
               buttons = 'excel',
               scrollX = TRUE,
               fixedColumns = TRUE,
               columnDefs = list(list(className = 'dt-center', 
                                      targets = "_all"))))

period = c("Anual", "Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
           "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")

server = function(input, output, session) {
  
  data = reactive({
    req(input$input_file)
    
    treat_file(input$input_file$datapath)
  })
  
  parb = reactive({
    progress = shiny::Progress$new(style = "old")
    progress$set(message = "Carregando", value = 0)
    
    on.exit(progress$close())
    
    updateProgress = function(detail = NULL) {
      progress$inc(amount = 1/13, detail = detail)
    }
    
    parameters_block(data(), input$input_year, input$input_method,
                     updateProgress)
  })
  
  tabgof = reactive({
    progress = shiny::Progress$new(style = "old")
    progress$set(message = "Carregando", value = 0)
    
    on.exit(progress$close())
    
    updateProgress = function(detail = NULL) {
      progress$inc(amount = 1/13, detail = detail)
    }
    
    table_gof(data(), input$input_year, parb(), input$input_method, 
              updateProgress)
  })
  
  threshold = reactive({
    req(input$input_file)
    req(input$input_method == 2)
    
    data = treat_data(data())
    
    ts(input$input_threshold, data)
  })
  
  output$output_threshold <- renderUI({
    if(req(input$input_method) == 2)
      textInput(
        inputId = "input_threshold",
        label = "Aqui você pode inserir o limiar em quartil, com o sinal de 
        porcentagem (por exemplo, 90%), ou na unidade de medida da variável (por 
        exemplo, se sua variável for chuva e o limiar for 70.5 mm, basta 
        inserir 70.5). Por padrão, o limiar será o quartil 75%.",
        value = NULL,
        width = 550)
  })
  
  output$output_stcs = DT::renderDataTable({
    progress = shiny::Progress$new(style = "old")
    progress$set(message = "Carregando", value = 0)
    
    on.exit(progress$close())
    
    updateProgress = function(detail = NULL) {
      progress$inc(amount = 1/13, detail = detail)
    }
    
    stcs = calculate_stcs(data(), input$input_method, threshold(), 
                          updateProgress)
    
    DT::datatable(
      data = data.frame(stcs[[1]], stcs[[2]], round(unlist(stcs[[3]]), 2), 
                        round(unlist(stcs[[4]]), 2), round(unlist(stcs[[5]]), 2),
                        round(unlist(stcs[[6]]), 2), round(unlist(stcs[[7]]), 2), 
                        round(unlist(stcs[[8]]), 2), round(unlist(stcs[[9]]), 2), 
                        round(unlist(stcs[[10]]), 2), round(unlist(stcs[[11]]), 2), 
                        round(unlist(stcs[[12]]), 2), round(unlist(stcs[[13]]), 2), 
                        round(unlist(stcs[[14]]), 2), round(unlist(stcs[[15]]), 2), 
                        round(unlist(stcs[[16]]), 2)),
      colnames = c("nobs", "NAs", "Mínimo", "Máximo", "1. Quartil", 
                   "3. Quartil", "Média", "Mediana", "Somatório", "EP Média",
                   "LIC Média", "LSC Média", "Variância", "DP", "Assimetria",
                   "Curtose"),
      rownames = period,
      caption = htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: left;',
        htmltools::tags$ul(
          htmltools::tags$li('nobs: número de observações'),
          htmltools::tags$li('NAs: número de valores Não Aplicáveis (NAs)'),
          htmltools::tags$li('EP Média: Erro Padrão (EP) da Média'),
          htmltools::tags$li('LIC Média: Limite Inferior de Controle (LIC) da 
                             Média'),
          htmltools::tags$li('LSC Média: Limite Superior de Controle (LSC) da 
                             Média'),
          htmltools::tags$li('DP: Desvio Padrão')
          )
        ),
      extensions = c('Buttons', 'FixedColumns'))
  })
  
  output$output_pes = DT::renderDataTable({
    DT::datatable(
      data = data.frame(period, round(unlist(parb()[2]), 2),
                        round(unlist(parb()[3]), 2), round(unlist(parb()[4]), 2),
                        round(unlist(parb()[6]), 2), round(unlist(parb()[7]), 2)),
      colnames = c("Período","GVE.μ", "GVE.σ", "GVE.ξ", "Gumbel.μ", "Gumbel.σ"),
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: left;',
        htmltools::tags$ul(
          htmltools::tags$li('GVE.μ: parâmetro μ da distribuição Generalizada de 
                             Valores Extremos (GVE)'),
          htmltools::tags$li('GVE.σ: parâmetro σ da distribuição GVE'),
          htmltools::tags$li('GVE.ξ: parâmetro ξ da distribuição GVE'),
          htmltools::tags$li('Gumbel.μ: parâmetro μ da distribuição Gumbel'),
          htmltools::tags$li('Gumbel.σ: parâmetro σ da distribuição Gumbel')
        )
      ),
      extensions = 'Buttons')
  })
  
  output$output_ann = renderPlot({
    progress = shiny::Progress$new(style = "old")
    progress$set(message = "Carregando", value = 0)
    
    on.exit(progress$close())
    
    updateProgress = function(detail = NULL) {
      progress$inc(amount = 1/13, detail = detail)
    }
    
    generate_graphs(0, data(), input$input_year, period, input$input_name, 
                    input$input_uom, parb(), input$input_method, updateProgress)
  })
  
  output$output_jan = renderPlot({
    progress = shiny::Progress$new(style = "old")
    progress$set(message = "Carregando", value = 0)
    
    on.exit(progress$close())
    
    updateProgress = function(detail = NULL) {
      progress$inc(amount = 2/13, detail = detail)
    }
    
    generate_graphs(1, data(), input$input_year, period, input$input_name, 
                    input$input_uom, parb(), input$input_method, updateProgress)
  })
  
  output$output_feb = renderPlot({
    progress = shiny::Progress$new(style = "old")
    progress$set(message = "Carregando", value = 0)
    
    on.exit(progress$close())
    
    updateProgress = function(detail = NULL) {
      progress$inc(amount = 3/13, detail = detail)
    }
    
    generate_graphs(2, data(), input$input_year, period, input$input_name, 
                    input$input_uom, parb(), input$input_method, updateProgress)
  })
  
  output$output_mar = renderPlot({
    progress = shiny::Progress$new(style = "old")
    progress$set(message = "Carregando", value = 0)
    
    on.exit(progress$close())
    
    updateProgress = function(detail = NULL) {
      progress$inc(amount = 4/13, detail = detail)
    }
    
    generate_graphs(3, data(), input$input_year, period, input$input_name, 
                    input$input_uom, parb(), input$input_method, updateProgress)
  })
  
  output$output_apr = renderPlot({
    progress = shiny::Progress$new(style = "old")
    progress$set(message = "Carregando", value = 0)
    
    on.exit(progress$close())
    
    updateProgress = function(detail = NULL) {
      progress$inc(amount = 5/13, detail = detail)
    }
    
    generate_graphs(4, data(), input$input_year, period, input$input_name, 
                    input$input_uom, parb(), input$input_method, updateProgress)
  })
  
  output$output_may = renderPlot({
    progress = shiny::Progress$new(style = "old")
    progress$set(message = "Carregando", value = 0)
    
    on.exit(progress$close())
    
    updateProgress = function(detail = NULL) {
      progress$inc(amount = 6/13, detail = detail)
    }
    
    generate_graphs(5, data(), input$input_year, period, input$input_name, 
                    input$input_uom, parb(), input$input_method, updateProgress)
  })
  
  output$output_jun = renderPlot({
    progress = shiny::Progress$new(style = "old")
    progress$set(message = "Carregando", value = 0)
    
    on.exit(progress$close())
    
    updateProgress = function(detail = NULL) {
      progress$inc(amount = 7/13, detail = detail)
    }
    
    generate_graphs(6, data(), input$input_year, period, input$input_name, 
                    input$input_uom, parb(), input$input_method, updateProgress)
  })
  
  output$output_jul = renderPlot({
    progress = shiny::Progress$new(style = "old")
    progress$set(message = "Carregando", value = 0)
    
    on.exit(progress$close())
    
    updateProgress = function(detail = NULL) {
      progress$inc(amount = 8/13, detail = detail)
    }
    
    generate_graphs(7, data(), input$input_year, period, input$input_name, 
                    input$input_uom, parb(), input$input_method, updateProgress)
  })
  
  output$output_aug = renderPlot({
    progress = shiny::Progress$new(style = "old")
    progress$set(message = "Carregando", value = 0)
    
    on.exit(progress$close())
    
    updateProgress = function(detail = NULL) {
      progress$inc(amount = 9/13, detail = detail)
    }
    
    generate_graphs(8, data(), input$input_year, period, input$input_name, 
                    input$input_uom, parb(), input$input_method, updateProgress)
  })
  
  output$output_sep = renderPlot({
    progress = shiny::Progress$new(style = "old")
    progress$set(message = "Carregando", value = 0)
    
    on.exit(progress$close())
    
    updateProgress = function(detail = NULL) {
      progress$inc(amount = 10/13, detail = detail)
    }
    
    generate_graphs(9, data(), input$input_year, period, input$input_name, 
                    input$input_uom, parb(), input$input_method, updateProgress)
  })
  
  output$output_oct = renderPlot({
    progress = shiny::Progress$new(style = "old")
    progress$set(message = "Carregando", value = 0)
    
    on.exit(progress$close())
    
    updateProgress = function(detail = NULL) {
      progress$inc(amount = 11/13, detail = detail)
    }
    
    generate_graphs(10, data(), input$input_year, period, input$input_name, 
                    input$input_uom, parb(), input$input_method, updateProgress)
  })
  
  output$output_nov = renderPlot({
    progress = shiny::Progress$new(style = "old")
    progress$set(message = "Carregando", value = 0)
    
    on.exit(progress$close())
    
    updateProgress = function(detail = NULL) {
      progress$inc(amount = 12/13, detail = detail)
    }
    
    generate_graphs(11, data(), input$input_year, period, input$input_name, 
                    input$input_uom, parb(), input$input_method, updateProgress)
  })
  
  output$output_dec = renderPlot({
    progress = shiny::Progress$new(style = "old")
    progress$set(message = "Carregando", value = 0)
    
    on.exit(progress$close())
    
    updateProgress = function(detail = NULL) {
      progress$inc(amount = 13/13, detail = detail)
    }
    
    generate_graphs(12, data(), input$input_year, period, input$input_name, 
                    input$input_uom, parb(), input$input_method, updateProgress)
  })
  
  output$output_gof = DT::renderDataTable({
    DT::datatable(
      data = data.frame(
        period, round(unlist(tabgof()[1]), 4), round(unlist(tabgof()[2]), 4), 
        round(unlist(tabgof()[3]), 4), round(unlist(tabgof()[4]), 4), 
        round(unlist(tabgof()[5]), 4), round(unlist(tabgof()[6]), 2), 
        round(unlist(tabgof()[7]), 2)),
      
      colnames = c("Período","TMK", "TLB", "TRV", "TKS.GVE", "TKS.Gumbel", 
                   "EPAM.GVE", "EPAM.Gumbel"),
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: left;',
        htmltools::tags$ul(
          htmltools::tags$li('TMK: Teste de Mann-Kendall'),
          htmltools::tags$li('TLB: Teste de Ljung-Box'),
          htmltools::tags$li('TRV: Teste de Razão de Verossimilhança'),
          htmltools::tags$li('TKS.GVE: Teste de Kolmogorov-Smirnov (TKS) para a 
                             distribuição Generalizada de Valores Extremos (GVE)'),
          htmltools::tags$li('TKS.Gumbel: TKS para a distribuição Gumbel'),
          htmltools::tags$li('EPAM.GVE: Erro Percentual Absoluto Médio (EPAM) 
                             para a distribuição GVE'),
          htmltools::tags$li('EPAM.Gumbel: EPAM para a distribuição Gumbel')
        )
      ),
      extensions = 'Buttons')
  })
  
  output$output_probs = DT::renderDataTable({
    progress = shiny::Progress$new(style = "old")
    progress$set(message = "Carregando", value = 0)
    
    on.exit(progress$close())
    
    updateProgress = function(detail = NULL) {
      progress$inc(amount = 1/13, detail = detail)
    }
    
    lvls = as.numeric(unlist(strsplit(input$input_probs, ";")))
    rts = as.numeric(unlist(strsplit(input$input_rls, ";")))
    
    probs = probs_rls(data(), input$input_year, lvls, rts, input$input_sl, 
                      parb(), tabgof(), updateProgress)
    
    DT::datatable(
      data = data.frame(period, probs[[1]], round(probs[[2]][[1]], 2),
                        round(probs[[2]][[2]], 2), round(probs[[2]][[3]], 2),
                        round(probs[[2]][[4]], 2), round(probs[[2]][[5]], 2),
                        round(probs[[2]][[6]], 2), round(probs[[2]][[7]], 2),
                        round(probs[[2]][[8]], 2), round(probs[[2]][[9]], 2),
                        round(probs[[2]][[10]], 2), round(probs[[2]][[11]], 2),
                        round(probs[[2]][[12]], 2)),
      colnames = c("Período","Distribuição", lvls[1],
                   ifelse(is.na(lvls[2]), '', lvls[2]),
                   ifelse(is.na(lvls[3]), '', lvls[3]),
                   ifelse(is.na(lvls[4]), '', lvls[4]),
                   ifelse(is.na(lvls[5]), '', lvls[5]),
                   ifelse(is.na(lvls[6]), '', lvls[6]),
                   ifelse(is.na(lvls[7]), '', lvls[7]),
                   ifelse(is.na(lvls[8]), '', lvls[8]),
                   ifelse(is.na(lvls[9]), '', lvls[9]),
                   ifelse(is.na(lvls[10]), '', lvls[10]),
                   ifelse(is.na(lvls[11]), '', lvls[11]),
                   ifelse(is.na(lvls[12]), '', lvls[12])),
      rownames = FALSE,
      extensions = 'Buttons')
  })
  
  output$output_rls = DT::renderDataTable({
    progress = shiny::Progress$new(style = "old")
    progress$set(message = "Carregando", value = 0)
    
    on.exit(progress$close())
    
    updateProgress = function(detail = NULL) {
      progress$inc(amount = 1/13, detail = detail)
    }
    
    lvls = as.numeric(unlist(strsplit(input$input_probs, ";")))
    rts = as.numeric(unlist(strsplit(input$input_rls, ";")))
    
    rls = probs_rls(data(), input$input_year, lvls, rts, input$input_sl, 
                    parb(), tabgof(), updateProgress)
    
    DT::datatable(
      data = data.frame(period, rls[[1]], round(rls[[3]][[1]], 2),
                        round(rls[[3]][[2]], 2), round(rls[[3]][[3]], 2),
                        round(rls[[3]][[4]], 2), round(rls[[3]][[5]], 2),
                        round(rls[[3]][[6]], 2), round(rls[[3]][[7]], 2),
                        round(rls[[3]][[8]], 2), round(rls[[3]][[9]], 2),
                        round(rls[[3]][[10]], 2), round(rls[[3]][[11]], 2),
                        round(rls[[3]][[12]], 2)),
      colnames = c("Período","Distribuição", rts[1],
                   ifelse(is.na(rts[2]), '', rts[2]),
                   ifelse(is.na(rts[3]), '', rts[3]),
                   ifelse(is.na(rts[4]), '', rts[4]),
                   ifelse(is.na(rts[5]), '', rts[5]),
                   ifelse(is.na(rts[6]), '', rts[6]),
                   ifelse(is.na(rts[7]), '', rts[7]),
                   ifelse(is.na(rts[8]), '', rts[8]),
                   ifelse(is.na(rts[9]), '', rts[9]),
                   ifelse(is.na(rts[10]), '', rts[10]),
                   ifelse(is.na(rts[11]), '', rts[11]),
                   ifelse(is.na(rts[12]), '', rts[12])),
      rownames = FALSE,
      extensions = 'Buttons')
  })
}