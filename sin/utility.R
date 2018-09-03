# Global parameters
# b = 10
# c = 1
# g = h = 3
# pars = numeric(0)
# pars["g"] = pars["h"] = 1#3
# pars["c"] = 1; pars["b"] = 10
get_pars = local({
  pars = numeric(0)
  pars["g"] = pars["h"] = 3
  pars["c"] = 0.01; pars["b"] = 10
 function() pars
})

f = function(t, weight = 6*pi) exp(-0.5*t) * sin(weight*t)
theta_hat = function(t, y) {
  sum(f(t)*y)/sum(f(t) ^ 2)
}

tau_prior = function(n = 1, g, h) {
  rgamma(n, shape = g, rate = h)
}

theta_prior = function(n=1, b, c, h, g) {
  b + sqrt(h/(g*c)) * rt(n, df = 2*g)
}

# Can't use B due to ace not handling arguments correctly
B1 = function(c, b, t, y) {
  (b * c + P)/C(c, t)
}
C = function(c, t) c + Q(t)
Q = function(t) sum(f(t)^2)
G = function(g, y) g + length(y)/2
P = function(t, y) sum(f(t) * y)

H = function(c, b, h, g, t, y) {
  s_2 = sum((y - P(t, y)* f(t)/Q(t))^2)
  h + 1/2*( s_2 + b^2 * c + P(t, y) * c/ (Q(t)*C(c, t))
  )
}


simulate = function(t, theta=10, tau=10) {
  theta*f(t) + rnorm(length(t), 0, sd = sqrt(1/tau))
}

t = seq(0, 1, 0.001)

b = 10
c = 0.01
g = h = 3

theta = theta_prior(1e4, b, c, h, g)
tau = tau_prior(1e4, g, h)
setnicepar(mfrow = c(1, 2))
hist(theta, breaks= "fd")
hist(tau, breaks= "fd")

get_utility = function(c, b, h, g, t, y) {
  #  v = G(g, y) / ((G(g, y) - 1) * 
  #                  H(c, b, h, g, t, y) * 
  #                  C(c, t))
  # v = H(c, b, h, g, t, y)^3/(C(c, t)*(G(g, y) - 1)^3 * (G(g, y)-2))
  log(C(c, t)) - 3 * log(G(g, y))
  # 1/v
}



get_utility_new = function(t, pars) {
  g = pars["g"]; h = pars["h"]
  c = pars["c"]; b = pars["b"]
  tau = tau_prior(1, g, h)
  beta = theta_prior(1, b, c, h, g)
  sim = simulate(t, beta, tau)
  get_utility(c, b, h, g, t, sim)
}





