# 📌 Instalar paquetes si no están instalados
if (!require(shiny)) install.packages("shiny", dependencies=TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies=TRUE)

library(shiny)
library(ggplot2)

# 📌 Definir la interfaz de usuario (UI)
ui <- fluidPage(
  
  titlePanel("Calculadora de Distribución Normal"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      numericInput("mu", "Media (μ):", value = 15, min = -1000, max = 1000),
      numericInput("sigma", "Desviación estándar (σ):", value = 2, min = 0.1, max = 100),
      
      selectInput("operacion", "Seleccione una operación:", 
                  choices = c("P(X < a)" = "menor", 
                              "P(X > a)" = "mayor", 
                              "P(a < X < b)" = "entre")),
      
      numericInput("a", "Valor de a:", value = 16),
      
      conditionalPanel(
        condition = "input.operacion == 'entre'",
        numericInput("b", "Valor de b:", value = 20)
      ),
      
      actionButton("calcular", "Calcular Probabilidad")
    ),
    
    mainPanel(
      textOutput("resultado"),
      plotOutput("grafico")
    )
  )
)

# 📌 Definir la lógica del servidor
server <- function(input, output) {
  
  calcular_probabilidad <- eventReactive(input$calcular, {
    mu <- input$mu
    sigma <- input$sigma
    a <- input$a
    b <- input$b
    operacion <- input$operacion
    
    if (operacion == "menor") {
      p <- pnorm(a, mean=mu, sd=sigma)
      texto <- paste("P(X <", a, ")")
      x_fill <- seq(min(mu - 4*sigma, a - 2), a, length=100)
      text_x <- a - sigma
      
    } else if (operacion == "mayor") {
      p <- 1 - pnorm(a, mean=mu, sd=sigma)
      texto <- paste("P(X >", a, ")")
      x_fill <- seq(a, mu + 4*sigma, length=100)
      text_x <- a + sigma
      
    } else if (operacion == "entre") {
      p <- pnorm(b, mean=mu, sd=sigma) - pnorm(a, mean=mu, sd=sigma)
      texto <- paste("P(", a, "< X <", b, ")")
      x_fill <- seq(a, b, length=100)
      text_x <- (a + b) / 2
    }
    
    p_percent <- round(p * 100, 2)
    
    return(list(p=p_percent, texto=texto, x_fill=x_fill, text_x=text_x))
  })
  
  output$resultado <- renderText({
    req(calcular_probabilidad())
    res <- calcular_probabilidad()
    paste(res$texto, "=", res$p, "%")
  })
  
  output$grafico <- renderPlot({
    req(calcular_probabilidad())
    
    res <- calcular_probabilidad()
    mu <- input$mu
    sigma <- input$sigma
    x_fill <- res$x_fill
    text_x <- res$text_x
    
    x <- seq(mu - 4*sigma, mu + 4*sigma, length=200)
    y <- dnorm(x, mean=mu, sd=sigma)
    y_fill <- dnorm(x_fill, mean=mu, sd=sigma)
    
    ggplot(data.frame(x, y), aes(x, y)) +
      geom_line(color = "blue", linewidth = 1) +
      geom_area(data=data.frame(x=x_fill, y=y_fill), aes(x, y), fill="red", alpha=0.5) +
      ggtitle(paste("Distribución Normal N(", mu, ",", sigma, ")")) +
      xlab("Valores de X") + 
      ylab("Densidad") +
      theme_minimal() +
      annotate("text", x = text_x, y = max(y) * 0.2, 
               label = paste0("P = ", res$p, "%"), size = 5, color = "black", fontface = "bold")
  })
}

# 📌 Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)

