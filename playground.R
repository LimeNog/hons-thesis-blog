# for a random variable X with pdf p(X), the entropy is -E[log(p(X))]
# or sum over all possible values of X {- p(X) * log(p(X))}

sd <- 1
X <- seq(-5,5,0.001)

par(mfrow=c(1,2))

plot(X, dnorm(vals, mean=0, sd=sd), 
     type="l",
     ylim = c(0,1))
  
plot(X, - dnorm(vals, mean=0, sd=sd) * log(dnorm(vals, mean=0, sd=sd)), 
     type="l",
     ylim = c(0,1))




library(plotly)

fig <- plot_ly()

for (s in sigmas) {
  fig <- fig |>
    add_lines(x = X, y = dnorm(X,0,s),
              frame = round(s,2),
              name = "PDF") |>
    add_lines(x = X, y = -dnorm(X,0,s)*log(dnorm(X,0,s)),
              frame = round(s,2),
              name = "Entropy")
}

fig |> layout(sliders = list(list(active = 0)))
