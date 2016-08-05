from pulp import *
 
# declare your variables
x = LpVariable("X", 0, 1000,LpInteger)   # 0<= x1 <= 40
y = LpVariable("Y", 0, 1000,LpInteger) # 0<= x2 <= 1000
c = LpVariable("c", 0, 33,LpInteger)   # 0<= x1 <= 40
f = LpVariable("f", 0, 60,LpInteger) # 0<= x2 <= 1000
a = LpVariable("a", 0, 95,LpInteger)   # 0<= x1 <= 40

# defines the problem
prob = LpProblem("problem", LpMaximize)
 
# defines the constraints
prob += 5*x + 4*y <= c 
prob += 10*x + 5*y <= f
prob += 8*x + 14*y <=a
 
# defines the objective function to maximize
prob += 50*x+35*y
 
# solve the problem
status = prob.solve(GLPK(msg=0))
LpStatus[status]
print("Status:", LpStatus[prob.status])
 
# print the results x1 = 20, x2 = 60
value(x)
value(y)
value(c)
value(f)
value(a)

for v in prob.variables():
    print(v.name, "=", v.varValue)


print("Total Cost of Ingredients per can = ", value(prob.objective))

