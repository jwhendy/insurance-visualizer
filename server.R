library(shiny)
library(ggplot2)
library(markdown)
library(plyr)

load("./plans.rda")

converter <- function(expenses) {
  
  exp_ind <- max(expenses)
  exp_rem <- sum(expenses[-which(expenses == exp_ind)[1]])
  list("exp_ind" = exp_ind, "exp_rem" = exp_rem)
  
}

condition <- function(exp_ind, exp_rem, class) {
  
  compare <- plans[plans$class == class, ]
  
  test_case <- c(rep(c(exp_ind, exp_rem, exp_ind + exp_rem), each = 2))
  test_case <- rbind(test_case, test_case, test_case)
  
  limits <- cbind(compare$ded_ind, compare$exp_max_ind,
                  compare$ded_ind, compare$exp_max_ind, 
                  compare$ded_tot, compare$exp_max_tot)
  
  result <- cbind(compare[, c("plan", "ded_ind", "ded_tot", "oop_ind", "oop_tot", "prem", "hsa")],
                  exp_ind, exp_rem, (test_case > limits) %*% (2^(0:5)))
  names(result)[ncol(result)] <- "bin"
  return(result)
  
}

map_funcs <- list()
length(map_funcs) <- 17
map_funcs <- list(
  "0" = function(binary) { binary$exp_ind + binary$exp_rem }, 
  "1" = function(binary) { binary$ded_ind + (0.1* (binary$exp_ind - binary$ded_ind)) + binary$exp_rem }, 
  "4" = function(binary) { binary$exp_ind + binary$exp_rem }, 
  "16" = function(binary) { binary$ded_tot + (0.1 * (binary$exp_ind + binary$exp_rem - binary$ded_tot)) },
  "17" = function(binary) { binary$ded_ind + (0.1* (binary$exp_ind - binary$ded_ind)) + binary$exp_rem },
  "19" = function(binary) { binary$oop_ind + binary$exp_rem }, 
  "20" = function(binary) { binary$ded_tot + (0.1 * (binary$exp_ind + binary$exp_rem - binary$ded_tot)) }, 
  "21" = function(binary) { binary$ded_tot + (0.1 * (binary$exp_ind + binary$exp_rem - binary$ded_tot)) }, 
  "23" = function(binary) { binary$oop_ind + binary$ded_ind + (0.1 * (binary$exp_rem - binary$ded_ind)) },
  "28" = function(binary) { binary$ded_tot + (0.1 * (binary$exp_ind + binary$exp_rem - binary$ded_tot)) },
  "29" = function(binary) { binary$ded_tot + (0.1 * (binary$exp_ind + binary$exp_rem - binary$ded_tot)) },
  "48" = function(binary) { binary$oop_tot },   
  "51" = function(binary) { binary$oop_ind + binary$exp_rem }, 
  "55" = function(binary) { binary$oop_ind + binary$ded_ind + (0.1 * (binary$exp_rem - binary$ded_ind)) }, 
  "60" = function(binary) { binary$oop_tot }, 
  "61" = function(binary) { binary$oop_tot }, 
  "63" = function(binary) { binary$oop_tot }
)

global_min <- 0
global_max <- 500
global_members <- 1

shinyServer(function(input, output) {
  
  output$sliders <- renderUI({
    
    class <- input$class
    members <- input$members
    if(class == "emp") {
      members <- global_members
    }
    
    max_pred <- as.integer(input$max_pred)
    
    individuals <- lapply(1:members, function(i) {
      
      input_name <- paste0("ind", i)
      label_name <- paste("Individual", i)
      
      existing_min <- isolate(input[[input_name]][1])
      existing_max <- isolate(input[[input_name]][2])
      
      if(is.null(existing_min)) {
        existing_min <- global_min
        existing_max <- global_max  
      }
      
      sliderInput(inputId = input_name, label = label_name, min = 0, max = max_pred,
                  value = c(existing_min, existing_max), step = 100)
      
    })
    
    do.call(tagList, individuals)
    
  })
  
  expenses <- reactive({
    
    class <- input$class
    members <- as.numeric(input$members)
    if(class == "emp") {
      members <- global_members
    }
    
    if(is.null(input[[paste0("ind", members)]])) {
      return()
    }
    
    else{
      
      mins <- sapply(1:members, function(i) {
        as.numeric(input[[paste0("ind", i)]])[1]
      })
      
      maxs <- sapply(1:members, function(i) {
        as.numeric(input[[paste0("ind", i)]])[2]
      })
      
      expenses <- as.data.frame(cbind(mins, maxs))
    }
    
  })
  
  cases <- reactive({
    
    expenses <- expenses()
    if(is.null(expenses)) {
      binary <- condition(0, 0, input$class)
      binary$cost <- 0
      list(mins = binary, maxs = binary)
    }
    
    else{
      
      mins_list <- converter(expenses$mins)
      maxs_list <- converter(expenses$maxs)
      expense_list <- list(mins = mins_list, maxs = maxs_list)
      
      lapply(expense_list, function(exp_list) {
        
        binary <- condition(exp_list[[1]], exp_list[[2]], input$class)
        
        costs <- sapply(1:nrow(binary), function(i) {
          costs <- map_funcs[[as.character(binary[i, "bin"])]](binary[i, ])
          return(costs)
        }) 
        
        binary$cost <- costs

        return(binary)
        
      })
      
    }
    
  })
  
  output$main_plot <- renderPlot({
    
    hsa_vol <- input$hsa
    if(is.null(input[[paste0("ind", input$members)]])) {
      return()
    }
    
    else {
      
      generate_plot_data <- function(binary) {
        
        plot <- lapply(1:nrow(binary), function(i){
          temp <- binary[i, ]
          delta <- temp$cost - temp$hsa - hsa_vol
          plot <- data.frame(plan = rep(temp$plan, 4),
                             start = c(min(delta, 0), temp$prem, max(0, delta),
                                       c(temp$cost, temp$hsa + hsa_vol)[(delta > 0)+1]))
          plot[plot$plan == "A", "start"][1] <- 0
          offsets <- c(0, cumsum(plot$start[2:4]))
          plot$end <- offsets - abs(plot$start)
          plot$start <- offsets
          plot$fill <- factor(c("c", "b", "a", "a"))
          plot$alpha <- factor(c(1, 1, 1, 0))
          return(plot)
        } )
        
      }
      
      plot_min <- generate_plot_data(cases()[[1]])
      plot_max <- generate_plot_data(cases()[[2]])
      
      plot <- rbind(do.call(rbind, plot_min), do.call(rbind, plot_max))
      plot$case <- c(rep("Best case", nrow(plot)/2), rep("Worst case", nrow(plot)/2))
      plot$plan <- factor(plot$plan, levels = c("C", "B", "A"))
      
      plot$fill <- factor(plot$fill, levels = c(as.character(unique(plot$fill)), "d"))
      
      p <- ggplot(plot, aes(x = plan, xend = plan, y = start, yend = end, colour = fill, alpha = alpha))
      p <- p + geom_segment(size = 35) + theme_bw() + coord_flip() + facet_wrap(~case, ncol = 2)
      p <- p + scale_alpha_discrete(range = c(0.35, 1), guide = F)
      p <- p + scale_colour_manual("Annual Cost", limits = c("a", "b", "c", "d"),
                                   labels = c("Expenses", "Premiums", "Carry-over HSA", "Expenses paid \n from HSA/HCRA"),
                                   values = hcl(c(15, 255, 135, 15), l=65, c=100, alpha = c(1, 1, 1, 0.35)))
      p <- p + scale_y_continuous(limits = c(min(c(plot$start, plot$end)), max(c(plot$start, plot$end))),
                                  breaks = c(seq(round_any(min(c(plot$start, plot$end)), 500, f = floor), max(plot$end, plot$start), by = 500)))
      p <- p + theme(axis.title = element_blank(), text = element_text(size = 20),
                     axis.text.x = element_text(angle = 315, hjust = 0))
      p <- p + guides(colour = guide_legend(override.aes = list(size = 7)))
      print(p)
    }
  })
  
  output$bar_summ_best <- renderTable({
    
    best_case <- cases()[["mins"]]
    
    hsa_vol <- input$hsa
    
    summary <- best_case[, c("plan", "exp_ind", "exp_rem")]
    
    a <- best_case[best_case$plan == "A", ]
    if(a$cost < (a$hsa + hsa_vol)) {summary[summary$plan == "A", "cost"] <- a$prem }
    
    summary$cost <- best_case$cost + best_case$prem - best_case$hsa - hsa_vol
    summary$relative <- summary$cost - max(summary$cost)
    names(summary) <- c("Plan", "Max spender", "Others",
                        "Expenses + premiums - HSA", "Compared to max")
    print(summary)
    
  }, include.rownames = F)
  
  output$bar_summ_worst <- renderTable({
    
    worst_case <- cases()[["maxs"]]
    
    hsa_vol <- input$hsa
    
    summary <- worst_case[, c("plan", "exp_ind", "exp_rem")]
    
    a <- worst_case[worst_case$plan == "A", ]
    if(a$cost < (a$hsa + hsa_vol)) {summary[summary$plan == "A", "cost"] <- a$prem }
    
    summary$cost <- worst_case$cost + worst_case$prem - worst_case$hsa - hsa_vol
    summary$relative <- summary$cost - max(summary$cost)
    names(summary) <- c("Plan", "Max spender", "Others",
                        "Expenses + premiums - HSA", "Compared to max")
    print(summary)
    
  }, include.rownames = F)
  
  output$flow_ave <- renderPlot({
    if(is.null(input[[paste0("ind", input$members)]])) {
      return()
    }
    
    else{
      
      expenses <- expenses()
      hsa_vol <- input$hsa
      mins <- expenses$mins
      maxs <- expenses$maxs    
      
      generator <- function(exp) {  
        exp_list <- converter(exp)
        monthly <- data.frame(month = month.name,
                              exp_ind = cumsum(rep(exp_list[[1]], 12)/12), 
                              exp_rem = cumsum(rep(exp_list[[2]], 12)/12))
        
        binary_list <- lapply(1:nrow(monthly), function(i) {
          
          binary <- condition(monthly[i, "exp_ind"], monthly[i, "exp_rem"], input$class)
       
          costs <- sapply(1:nrow(binary), function(i) {
            costs <- map_funcs[[as.character(binary[i, "bin"])]](binary[i, ])
            return(costs)
          }) 
          
          binary$cost <- costs
          
          binary$month <- monthly[i, "month"]
          return(binary)
        })
        
        binary <- do.call(rbind, binary_list)
        
        plot <- binary[, c("plan", "month", "prem", "cost", "hsa")]
        plot$prem_ave <- binary$prem / 12      
        
        plot$month <- factor(plot$month, levels = month.name)
        
        hsa_split <- split(plot$hsa, plot$plan)
        
        hsas_ave <- lapply(hsa_split[c("B", "C")], function(x) {
          x + cumsum(rep(hsa_vol/12, 12))
        })
        
        hsa_cum <- c(rep(hsa_vol, 12), hsas_ave[[1]], hsas_ave[[2]])
        
        plot <- ddply(plot, .(plan), mutate,
                      prem_cum = cumsum(prem/12))

        plot <- plot[order(plot$plan, decreasing = T), ]
        
        plot$hsa_cum <- hsa_cum
        plot$delta <- plot$cost - plot$hsa_cum
        
        plot <- ddply(plot, .(plan, month), mutate,
                      total = prem_cum + c(delta, 0)[(delta < 0) + 1])
        plot$delta[plot$delta > 0] <- 0
        plot[plot$plan == "A" & plot$month == "December", "delta"] <- 0
        
        plot <- melt(plot, id.vars = c("plan", "month"), measure.vars = c("total", "delta"))
        return(plot)
      }
      
      plot <- rbind(generator(mins), generator(maxs))
      plot$case <- c(rep("Best case", nrow(plot)/2), rep("Worst case", nrow(plot)/2))
      
      plot$plan <- factor(plot$plan, levels = c("A", "B", "C"))
      
      q <- ggplot(plot[plot$variable == "total", ],
                  aes(x = month, y = value, colour = plan, group = plan))
      q <- q + facet_grid(~case)
      q <- q + geom_hline(yintercept = 0, linetype = 2, alpha = 0.5, size = 0.75)
      q <- q + geom_point() + geom_line()
      q <- q + geom_ribbon(data = plot[plot$variable == "delta", ], 
                           aes(ymin = value, ymax = 0, group = plan, fill = plan), alpha = 0.1)
      q <- q + theme_bw()  
      q <- q + scale_alpha_discrete("Type", limits = c("total", "delta"),
                                    labels = c("Running cost", "HSA/HCRA"),
                                    range = c(1, 0.35))
      q <- q + theme(axis.title = element_blank(), text = element_text(size = 20),
                     axis.text.x = element_text(angle = 315, hjust = 0))
      print(q)
    }
  })
  
  output$flow_jan <- renderPlot({
    if(is.null(input[[paste0("ind", input$members)]])) {
      return()
    }
    
    else{
      
      expenses <- expenses()
      hsa_vol <- input$hsa
      mins <- expenses$mins
      maxs <- expenses$maxs    
      
      generator <- function(exp) {  
        exp_list <- converter(exp)
        
        binary <- condition(exp_list[[1]], exp_list[[2]], input$class)
              
        costs <- sapply(1:nrow(binary), function(i) {
          costs <- map_funcs[[as.character(binary[i, "bin"])]](binary[i, ])
          return(costs)
        }) 
        
        binary$cost <- costs
            
        plot_mini <- binary[, c("plan", "prem", "cost", "hsa")]
        plot_mini$prem_ave <- binary$prem / 12      
        plot <- plot_mini[rep(1:3, 12), ]
        
        plot$month <- factor(rep(month.name, each = 3), levels = month.name)
        
        hsa_split <- split(plot$hsa, plot$plan)
        
        hsas_ave <- lapply(hsa_split[c("B", "C")], function(x) {
          x + cumsum(rep(hsa_vol/12, 12))
        })
        
        hsa_cum <- c(rep(hsa_vol, 12), hsas_ave[[1]], hsas_ave[[2]])
        
        plot <- ddply(plot, .(plan), mutate,
                      prem_cum = cumsum(prem/12))

        plot <- plot[order(plot$plan, decreasing = T), ]
        
        plot$hsa_cum <- hsa_cum
        plot$delta <- plot$cost - plot$hsa_cum
        
        plot <- ddply(plot, .(plan, month), mutate,
                      total = prem_cum + c(delta, 0)[(delta < 0) + 1])
        plot$delta[plot$delta > 0] <- 0
        plot[plot$plan == "A" & plot$month == "December", "delta"] <- 0
        
        plot <- melt(plot, id.vars = c("plan", "month"), measure.vars = c("total", "delta"))
        return(plot)
      }
      
      plot <- rbind(generator(mins), generator(maxs))
      plot$case <- c(rep("Best case", nrow(plot)/2), rep("Worst case", nrow(plot)/2))
      
      plot$plan <- factor(plot$plan, levels = c("A", "B", "C"))
      
      r <- ggplot(plot[plot$variable == "total", ],
                  aes(x = month, y = value, colour = plan, group = plan))
      r <- r + facet_grid(~case)
      r <- r + geom_hline(yintercept = 0, linetype = 2, alpha = 0.5, size = 0.75)
      r <- r + geom_point() + geom_line()
      r <- r + geom_ribbon(data = plot[plot$variable == "delta", ], 
                           aes(ymin = value, ymax = 0, group = plan, fill = plan), alpha = 0.1)
      r <- r + theme_bw()  
      r <- r + scale_alpha_discrete("Type", limits = c("total", "delta"),
                                    labels = c("Running cost", "HSA/HCRA"),
                                    range = c(1, 0.35))
      r <- r + theme(axis.title = element_blank(), text = element_text(size = 20),
                     axis.text.x = element_text(angle = 315, hjust = 0))
      print(r)
    }
    
  })
  
})
