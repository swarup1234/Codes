from pulp import *

#trying to solve a get a magic square 
 
# declare your variables
N=5
I=range(1,N+1)
J=range(1,N+1)
K=range(1,N**2+1)
var_bles=[(i,j,k) for i in range(1,N+1) for j in range(1,N+1) for k in range(1,(N**2)+1)]
x = LpVariable.dicts("X",var_bles,0,1,LpInteger)	


# defines the problem
prob = LpProblem("Rahul", LpMinimize)

for i in I:
	prob += lpSum([k*x[i,j,k] for j in J for k in K])==(N*(N**2+1))/2

for j in I:
	prob += lpSum([k*x[i,j,k] for i in J for k in K])==(N*(N**2+1))/2

for i in I:
	for j in J:
		prob += lpSum([x[i,j,k] for k in K])==1

for k in K:
	prob += lpSum([x[i,j,k] for i in I for j in J])==1



prob += lpSum([k*x[i,i,k] for i in I for k in K])==(N*(N**2+1))/2


# solving the problem using glpk solver 
status = prob.solve(GLPK(msg=0))
LpStatus[status]
print("Status:", LpStatus[prob.status])
 
# print the results
for v in prob.variables():
	if v.varValue == 1:
		print(v.name, "=", v.varValue)
count=0
flag=1
#outputing in a specified format
for v in prob.variables():
	if v.varValue == 1:
		print v.name[9:11],
		count+=1
		flag = 0
	if count%N==0 and flag == 0:
		print 
		flag=1



