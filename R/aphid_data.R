##lambda=1.75, mu=0.00095
sim = read.table(
textConnection("Time N C
0.0 30 36
1.14 225 247
2.29 815 1272
3.57 511 3057
4.57 120 3543"), header=TRUE)

diff(sim$Time)

