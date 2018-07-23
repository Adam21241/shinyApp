library(shiny)

#library(pacman)

#p_install("caret")
#p_install("class")
#p_install("party")
#p_install("e1071")



ui <- fluidPage(
  
  titlePanel(h1("PORÓWNANIE METOD KLASYFIKACJI"), "Porównanie metod klasyfikacji"),
  br(),br(),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("dataInput", "Wybierz zestaw danych: ",
                  choices = c("IRIS", "INDIAN", "ABALONE")),
      br(),
      
      sliderInput("neighbours", "[KNN] Liczba sąsiadów: ", min = 1, max = 10, value = 5),
      
      br(),
      
      conditionalPanel(
        "input.dataInput == 'IRIS'",
        sliderInput("minsplit1", "[CTREE] Parametr minsplit: ", min = 1, max = 60, value = 10) # 150 attributes
      ),
      conditionalPanel(
        "input.dataInput == 'INDIAN'",
        sliderInput("minsplit2", "[CTREE] Parametr minsplit: ", min = 1, max = 255, value = 20) # 768 attributes
      ),
      conditionalPanel(
        "input.dataInput == 'ABALONE'",
        sliderInput("minsplit3", "[CTREE] Parametr minsplit: ", min = 1, max = 1392, value = 100) # 4177 attributes
      ),
      
      br(),
      
      sliderInput("laplace", "[NB] Parametr Laplace'a: ", min = 0, max = 100, value = 10)
    
    ),
    
    mainPanel(
      plotOutput("header"),
      br(),
      tableOutput("header2")
    )
  )
)


###


server <- function(input, output) {
  
  indian <- read.csv("indian.csv")
  abalone <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data")
  
  observe({
    
    ### IRIS
    
    set.seed(123)
    iris_ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
    
    # iris && knn
    
    iris.training <- iris[iris_ind==1, 1:4]
    iris.test <- iris[iris_ind==2, 1:4]
    iris.trainLabels <- iris[iris_ind==1, 5]
    iris.testLabels <- iris[iris_ind==2, 5]
    library(class)
    iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k = input$neighbours)
    library(caret)
    x <- confusionMatrix(iris_pred, iris.testLabels)$overall[1]
    m <- confusionMatrix(iris_pred, iris.testLabels)$byClass[ ,1:2] 
    x <- round(x, 2)
    m <- round(m, 2)
    
    # iris && ctree
    
    iris.training <- iris[iris_ind==1, ]   # nowy zbiór trenujący, który zawiera też kolumnę Species
    iris.test <- iris[iris_ind==2, ]   # analogicznie testujący
    library(party)
    iris_pred2 <- ctree(Species ~., data = iris.training, controls = ctree_control(minsplit = input$minsplit1))
    iris_testPred2 <- predict(iris_pred2, newdata = iris.test)
    library(caret)
    y <- confusionMatrix(iris_testPred2,iris.testLabels)$overall[1] 
    n <- confusionMatrix(iris_testPred2, iris.testLabels)$byClass[ ,1:2] 
    y <- round(y, 2)
    n <- round(n, 2)
    
    # iris && naiveBayes
    
    library(e1071)
    iris_pred3 <- naiveBayes(Species ~ ., data = iris.training, laplace = input$laplace)
    iris_testPred3 <- predict(iris_pred3, newdata = iris.test, type = "class")
    library(caret)
    z <- confusionMatrix(iris_testPred3,iris.testLabels)$overall[1]
    o <- confusionMatrix(iris_testPred3, iris.testLabels)$byClass[ ,1:2]
    z <- round(z, 2)
    o <- round(o, 2)
    
    
    ### INDIAN
    
    names(indian) <- c("pregnant", "glucose", "pressure", "triceps", "insulin", "mass", "pedigree", "age", "diabetes")
    indian$diabetes <- as.factor(indian$diabetes)
    levels(indian$diabetes) <- c("negative", "positive")
    
    set.seed(123)
    indian_ind <- sample(2, nrow(indian), replace=TRUE, prob=c(0.67, 0.33))
    
    # indian && knn 
    
    indian.training <- indian[indian_ind==1, 1:8]
    indian.test <- indian[indian_ind==2, 1:8]
    indian.trainLabels <- indian[indian_ind==1, 9]
    indian.testLabels <- indian[indian_ind==2, 9]
    library(class)
    indian_pred <- knn(train = indian.training, test = indian.test, cl = indian.trainLabels, k = input$neighbours)
    library(caret)
    x1 <- confusionMatrix(indian_pred, indian.testLabels)$overall[1]
    m1 <- confusionMatrix(indian_pred, indian.testLabels)$byClass[1:2]
    x1 <- round(x1, 2)
    m1 <- round(m1, 2)
    
    ## indian && ctree
    
    indian.training <- indian[indian_ind==1, ]  
    indian.test <- indian[indian_ind==2, ]   
    library(party)
    indian_pred2 <- ctree(diabetes ~., data = indian.training, controls = ctree_control(minsplit = input$minsplit2))
    indian_testPred2 <- predict(indian_pred2, newdata = indian.test)
    y1 <- confusionMatrix(indian_testPred2,indian.testLabels)$overall[1]
    n1 <- confusionMatrix(indian_testPred2, indian.testLabels)$byClass[1:2]
    y1 <- round(y1, 2)
    n1 <- round(n1, 2)
    
    # indian && naiveBayes
    
    library(e1071)
    indian_pred3 <- naiveBayes(diabetes ~ ., data = indian.training, laplace = input$laplace)
    indian_testPred3 <- predict(indian_pred3, newdata = indian.test, type = "class")
    z1 <- confusionMatrix(indian_testPred3, indian.testLabels)$overall[1]
    o1 <- confusionMatrix(indian_testPred3, indian.testLabels)$byClass[1:2]
    z1 <- round(z1, 2)
    o1 <- round(o1, 2)
    
    
    ### ABALONE
    
    names(abalone) <- c("Type", "LongestShell", "Diameter", "Height", "WholeWeight", "ShuckedWeight", "VisceraWeight", "ShellWeight", "Rings")
    head(abalone)
    levels(abalone$Type) <- c("Female", "Infant", "Male")
    
    set.seed(123)
    abalone_ind <- sample(2, nrow(abalone), replace=TRUE, prob=c(0.67, 0.33))
    
    # abalone && knn
    
    abalone.training <- abalone[abalone_ind==1, 2:9]
    abalone.test <- abalone[abalone_ind==2, 2:9]
    abalone.trainLabels <- abalone[abalone_ind==1, 1]
    abalone.testLabels <- abalone[abalone_ind==2, 1]
    library(class)
    abalone_pred <- knn(train = abalone.training, test = abalone.test, cl = abalone.trainLabels, k = input$neighbours)
    library(caret)
    x2 <- confusionMatrix(abalone_pred, abalone.testLabels)$overall[1]
    m2 <- confusionMatrix(abalone_pred, abalone.testLabels)$byClass[ ,1:2]
    x2 <- round(x2, 2)
    m2 <- round(m2, 2)
    
    # abalone && ctree
    
    abalone.training <- abalone[abalone_ind==1, ]  
    abalone.test <- abalone[abalone_ind==2, ]   
    library(party)
    abalone_pred2 <- ctree(Type ~., data = abalone.training, controls = ctree_control(minsplit = input$minsplit3))
    abalone_testPred2 <- predict(abalone_pred2, newdata = abalone.test)
    y2 <- confusionMatrix(abalone_testPred2, abalone.testLabels)$overall[1]
    n2 <- confusionMatrix(abalone_testPred2, abalone.testLabels)$byClass[ ,1:2]
    y2 <- round(y2, 2)
    n2 <- round(n2, 2)
    
    # abalone && naiveBayes
    
    library(e1071)
    abalone_pred3 <- naiveBayes(Type ~ ., data = abalone.training, laplace = input$laplace)
    abalone_testPred3 <- predict(abalone_pred3, newdata = abalone.test, type = "class")
    z2 <- confusionMatrix(abalone_testPred3, abalone.testLabels)$overall[1]
    o2 <- confusionMatrix(abalone_testPred3, abalone.testLabels)$byClass[ ,1:2]  
    z2 <- round(z2, 2)
    o2 <- round(o2, 2)
  
    
    
    ###
    
    output$header <- renderPlot({
      if(input$dataInput == "IRIS") {
        v <- c(x,y,z)
        wykres <- barplot(v, main = "Accuracy for IRIS dataset", names = c("knn", "ctree", "naive Bayess"), ylim = c(0,1), las = 1)
        text(x = wykres, y = v, label = v, pos = 1, cex = 2.5, col = "red")
      }
      else if(input$dataInput == "INDIAN") {
        v1 <- c(x1,y1,z1)
        wykres <- barplot(v1, main = "Accuracy for INDIAN dataset", names = c("knn", "ctree", "naive Bayess"), ylim = c(0,1), las = 1)
        text(x = wykres, y = v1, label = v1, pos = 1, cex = 2.5, col = "red")
      }
      else {
        v2 <- c(x2,y2,z2)
        wykres <- barplot(v2, main = "Accuracy for ABALONE dataset", names = c("knn", "ctree", "naive Bayess"), ylim = c(0,1), las = 1)
        text(x = wykres, y = v2, label = v2, pos = 1, cex = 2.5, col = "red")
      }
    })
    
    ###
    
    if(input$dataInput == "IRIS") {
      output$header2 <- renderTable({
        labels <- c("setosa", "versicolor", "virginica")
        a <- cbind(labels, m, n, o)
        colnames(a) <- c(" ", "knn_sensitivity", "knn_specificity", "ctree_sensitivity", "ctree_specificity","NB_sensitivity", "NB_specificity")
        a
      })
    }
    else if(input$dataInput == "INDIAN") {
      output$header2 <- renderTable({
        labels <- c("Sensitivity", "Specificity")
        b <- cbind(labels, m1, n1, o1)
        colnames(b) <- c(" ", "knn", "ctree", "NB")
        b
      })
    }
    else {
      output$header2 <- renderTable({
        labels <- c("female", "male", "infant")
        c <- cbind(labels, m2, n2, o2)
        colnames(c) <- c(" ", "knn_sensitivity", "knn_specificity", "ctree_sensitivity", "ctree_specificity","NB_sensitivity", "NB_specificity")
        c
      })
    }
      
  })
}

shinyApp(ui = ui, server = server)
