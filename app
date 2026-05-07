library(shiny)
library(dplyr)
library(pROC)
library(bslib)

# =========================
# CARREGAR MODELO FINAL
# =========================
# Salve antes com algo como:
# saveRDS(model_bruto, "modelo_readmissao_admissao.rds")
model_bruto <- readRDS("modelo_readmissao_admissao.rds")

# =========================
# DADOS DO MODELO PARA ROC
# =========================
dados_auc <- model.frame(model_bruto)

roc_obj <- roc(
    response = dados_auc$readm_bin,
    predictor = fitted(model_bruto)
)

auc_val <- as.numeric(auc(roc_obj))

# =========================
# UI
# =========================
ui <- page_fluid(
    theme = bs_theme(
        version = 5,
        bootswatch = "flatly",
        base_font = font_google("Inter"),
        heading_font = font_google("Inter"),
        primary = "#0F5C7A",
        secondary = "#6c757d",
        success = "#2E8B57",
        warning = "#D4A017",
        danger = "#B22222",
        bg = "#F7F9FB",
        fg = "#1F2937"
    ),
    
    tags$head(
        tags$style(HTML("\n      .app-title {\n        font-size: 2rem;\n        font-weight: 800;\n        color: #0F5C7A;\n        margin-bottom: .25rem;\n      }\n      .app-subtitle {\n        color: #5B6470;\n        margin-bottom: 1rem;\n      }\n      .result-box {\n        background: #FFFFFF;\n        border-radius: 18px;\n        padding: 18px 20px;\n        border: 1px solid #E5E7EB;\n        box-shadow: 0 6px 18px rgba(15, 23, 42, 0.06);\n        margin-bottom: 1rem;\n      }\n      .prob-box {\n        font-size: 1.15rem;\n        font-weight: 700;\n        color: #0F172A;\n      }\n      .risk-chip {\n        padding: 14px 18px;\n        border-radius: 14px;\n        color: white;\n        font-size: 1.6rem;\n        font-weight: 800;\n        display: inline-block;\n        margin-top: .5rem;\n        margin-bottom: .5rem;\n      }\n      .section-title {\n        font-size: 1.15rem;\n        font-weight: 800;\n        color: #0F172A;\n        margin-bottom: .5rem;\n      }\n      .help-note {\n        font-size: 0.95rem;\n        color: #5B6470;\n      }\n      .sidebar-card .form-group {\n        margin-bottom: 1rem;\n      }\n      .btn-primary {\n        font-weight: 700;\n        border-radius: 12px;\n        padding: 10px 18px;\n      }\n      .nav-tabs .nav-link {\n        font-weight: 700;\n      }\n    "))
    ),
    
    div(
        style = "display:flex; align-items:center; gap:16px; margin-bottom:18px;",
        tags$img(
            src = "logo_readmissao.png",
            height = "90px"
        ),
        div(
            div(class = "app-title", "Previsor de Readmissão na UTI"),
            div(class = "app-subtitle", 
                "Ferramenta de apoio à decisão clínica baseada em modelo preditivo de admissão em UTI")
        )
    ),
    
    navset_card_tab(
        id = "tabs",
        height = "100%",
        
        nav_panel(
            "Calculadora",
            layout_columns(
                col_widths = c(4, 8),
                
                card(
                    class = "sidebar-card",
                    full_screen = FALSE,
                    card_header(strong("Dados clínicos de entrada")),
                    card_body(
                        numericInput("idade", "Idade (anos)", value = 60, min = 18, max = 120),
                        
                        selectInput(
                            "sexo", "Sexo",
                            choices = c("Feminino" = 0, "Masculino" = 1),
                            selected = 1
                        ),
                        
                        sliderInput(
                            inputId = "fragilidade",
                            label = "Fragilidade (ECF)",
                            min = 1,
                            max = 9,
                            value = 3,
                            step = 1,
                            ticks = TRUE
                        ),
                        numericInput("sofa", "SOFA na admissão", value = 2, min = 0, max = 24),
                        
                        selectInput(
                            "procedencia", "Procedência",
                            choices = c("Centro Cirúrgico", "Enfermaria", "Hemodinâmica", "Pronto Atendimento"),
                            selected = "Centro Cirúrgico"
                        ),
                        
                        selectInput(
                            "tipo_de_internamento", "Tipo de internamento",
                            choices = c("Cirúrgico de Emergência", "Cirúrgico Eletivo", "Clínico"),
                            selected = "Cirúrgico de Emergência"
                        ),
                        
                        selectInput(
                            "convenio", "Fonte pagadora",
                            choices = c("Não SUS" = 0, "SUS" = 1),
                            selected = 0
                        ),
                        
                        actionButton("calcular", "Calcular risco", class = "btn-primary")
                    )
                ),
                
                card(
                    full_screen = FALSE,
                    card_header(strong("Resultado da estimativa")),
                    card_body(
                        div(class = "result-box",
                            div(class = "section-title", "Probabilidade estimada"),
                            div(class = "prob-box", textOutput("prob_texto", inline = TRUE))
                        ),
                        div(class = "result-box",
                            div(class = "section-title", "Faixa de risco"),
                            uiOutput("faixa_risco")
                        ),
                        div(class = "result-box",
                            div(class = "section-title", "Interpretação clínica"),
                            htmlOutput("interpretacao")
                        ),
                        div(class = "result-box",
                            div(class = "section-title", "Taxa observada no estudo"),
                            textOutput("taxa_observada")
                        ),
                        div(class = "help-note",
                            "Esta calculadora tem finalidade de apoio à decisão clínica e não substitui o julgamento médico.")
                    )
                )
            )
        ),
        
        nav_panel(
            "Desempenho do modelo",
            layout_columns(
                col_widths = c(4, 8),
                
                card(
                    full_screen = FALSE,
                    card_header(strong("Resumo do desempenho")),
                    card_body(
                        div(class = "result-box",
                            div(class = "section-title", "Acurácia global"),
                            tags$p(strong(paste0("AUC do modelo: ", round(auc_val, 3))))
                        ),
                        div(class = "result-box",
                            div(class = "section-title", "Interpretação"),
                            tags$p("Discriminação moderada, com boa capacidade de estratificação clínica."),
                            tags$p("Calibração: boa, com erro absoluto médio aproximado de 0,004 na validação interna por bootstrap.")
                        ),
                        div(class = "help-note",
                            "A curva ROC e a AUC refletem o desempenho global do modelo na amostra de desenvolvimento, não a acurácia de um caso individual.")
                    )
                ),
                
                card(
                    full_screen = FALSE,
                    card_header(strong("Curva ROC")),
                    card_body(
                        plotOutput("roc_plot", height = "480px")
                    )
                )
            )
        )
    )
)

# =========================
# SERVER
# =========================
server <- function(input, output, session) {
    
    novo_dado <- eventReactive(input$calcular, {
        df <- data.frame(
            idade = input$idade,
            sexo = as.numeric(input$sexo),
            fragilidade_number = input$fragilidade,
            sofa = input$sofa,
            procedencia = input$procedencia,
            tipo_de_internamento = input$tipo_de_internamento,
            convenio = as.numeric(input$convenio),
            stringsAsFactors = FALSE,
            check.names = FALSE
        )
        
        names(df)[names(df) == "idade"] <- "Idade (anos)"
        
        df$procedencia <- factor(
            df$procedencia,
            levels = c("Centro Cirúrgico", "Enfermaria", "Hemodinâmica", "Pronto Atendimento")
        )
        
        df$tipo_de_internamento <- factor(
            df$tipo_de_internamento,
            levels = c("Cirúrgico de Emergência", "Cirúrgico Eletivo", "Clínico")
        )
        
        df
    })
    
    probabilidade <- eventReactive(input$calcular, {
        predict(model_bruto, newdata = novo_dado(), type = "response")
    })
    
    faixa <- reactive({
        req(probabilidade())
        if (probabilidade() < 0.05) {
            "Baixo risco"
        } else if (probabilidade() < 0.10) {
            "Risco intermediário"
        } else {
            "Alto risco"
        }
    })
    
    output$prob_texto <- renderText({
        req(probabilidade())
        paste0("Risco estimado: ", round(probabilidade() * 100, 1), "%")
    })
    
    output$faixa_risco <- renderUI({
        req(faixa())
        
        cor <- case_when(
            faixa() == "Baixo risco" ~ "#2E8B57",
            faixa() == "Risco intermediário" ~ "#D4A017",
            TRUE ~ "#B22222"
        )
        
        tags$div(
            class = "risk-chip",
            style = paste0("background-color:", cor, ";"),
            faixa()
        )
    })
    
    output$interpretacao <- renderUI({
        req(faixa())
        
        texto <- case_when(
            faixa() == "Baixo risco" ~
                "Paciente com menor probabilidade estimada de readmissão. Manter seguimento clínico usual.",
            faixa() == "Risco intermediário" ~
                "Paciente com risco intermediário. Considerar reavaliação clínica criteriosa antes da alta e vigilância após transferência.",
            TRUE ~
                "Paciente com risco elevado de readmissão. Recomenda-se reavaliar estabilidade clínica, planejamento da alta e necessidade de monitorização intensificada."
        )
        
        HTML(texto)
    })
    
    output$roc_plot <- renderPlot({
        plot(
            roc_obj,
            col = "#0F5C7A",
            lwd = 3,
            main = "Curva ROC - Modelo de Admissão",
            legacy.axes = TRUE
        )
        abline(a = 0, b = 1, lty = 2, col = "gray60")
    })
    
    taxas <- list(
        "Baixo risco" = list(p = 0.043, n = "62/1449"),
        "Risco intermediário" = list(p = 0.077, n = "413/5398"),
        "Alto risco" = list(p = 0.114, n = "203/1776")
    )
    
    output$taxa_observada <- renderText({
        req(faixa())
        
        info <- taxas[[faixa()]]
        
        paste0(
            "Pacientes classificados como ",
            faixa(),
            " apresentaram aproximadamente ",
            round(info$p * 100, 1),
            "% de readmissão (", info$n, ") na base do estudo."
        )
    })
}

# =========================
# APP
# =========================
shinyApp(ui, server)

