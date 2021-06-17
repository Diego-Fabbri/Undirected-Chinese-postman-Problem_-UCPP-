#Set your own working directory
setwd("C:/Users/diego/Documents/R/Projects/GitHub_Projects/Optimization/Traveling Salesman Problem (ATSP)")

# Import lpSolve package
library(lpSolve)

#Import required packages (ompr)
library(dplyr)
library(ROI)
library(ROI.plugin.symphony)
library(ompr)
library(ompr.roi)

#Set costs (euclidean distances)
c <- matrix(c(10000, 33.6, 14, 40.9, 14.5, 11.5,
            34.7, 10000, 21.7, 13, 20.2, 23.4,
            14.8, 21.5, 10000, 29.3, 2, 3.9,
            41.7, 13.1, 29.4, 10000, 27.6, 30.3,
            15, 20.2, 2, 27.5, 10000, 3.9,
            12, 22.8, 2, 30.1, 4, 10000), nrow = n, byrow = TRUE)

#Set problem size
n <- nrow(c)

#Build Model
Model <- MIPModel() %>%
  add_variable(x[i,j], i = 1:n, j = 1:n, type = "binary") %>% #define variables
  set_bounds(x[i, i], ub = 0, i = 1:n) %>% #define x variables' bounds
  add_variable(u[i], i = 1:n, lb = 1, ub = n) %>%
  set_objective(sum_expr(c[i, j] * x[i, j], i = 1:n, j = 1:n), "min") %>% #define objective
  add_constraint(sum_expr(x[i, j], j = 1:n) == 1, i = 1:n) %>% #define constraints
  add_constraint(sum_expr(x[i, j], i = 1:n) == 1, j = 1:n) %>%
  add_constraint(u[i] >= 2, i = 2:n) %>% 
  add_constraint(u[i] - u[j] + 1 <= (n - 1) * (1 - x[i, j]), i = 2:n, j = 2:n) %>% 
  solve_model(with_ROI(solver = "symphony", verbosity = 1))

#Model summary
##Status
print(paste("Model status is:", Model$status))

##Objective Function
print(paste("Objective value:", objective_value(Model)))

for (a in 1:n) {
  for (b in 1:n) {
    tmp_x <- get_solution(Model, x[i, j]) %>%
      filter(variable == "x", i == a, j == b) %>%
      select(value)


    if (tmp_x != 0) {
      print(paste("--->x[", a, ",", b , "] =", tmp_x))
    }
  }
}



