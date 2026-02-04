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


px <- seq(0,1,0.01)
plot(px, -log(px),
     type="l")




## interactive



library(plotly)

X <- seq(-5, 5, 0.01)
sds <- seq(0.5, 1, 0.05)

fig <- plot_ly()

for (s in sds) {
  pdf <- dnorm(X, 0, s)
  ent <- -pdf * log(pdf)
  
  # Add PDF line for this frame
  fig <- fig |>
    add_lines(
      x = X, y = pdf,
      type = "scatter", mode = "lines",
      name = "PDF",
      line = list(color = "blue"),
      frame = paste0("s=", round(s,2))
    )
  
  # Add Entropy line for the same frame
  fig <- fig |>
    add_lines(
      x = X, y = ent,
      type = "scatter", mode = "lines",
      name = "Entropy",
      line = list(color = "red"),
      frame = paste0("s=", round(s,2))
    )
}

fig <- fig |> layout(
  title = "Normal PDF and Entropy",
  xaxis = list(title = "x"),
  yaxis = list(title = "Value")
) |> animation_opts(
  frame = 100, transition = 0, redraw = FALSE
)

fig
