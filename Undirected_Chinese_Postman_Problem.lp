\ENCODING=ISO-8859-1
\Problem name: ilog.cplex

Minimize
 obj: x(1)(2)#0 + 5 x(1)(3)#1 + 4 x(1)(4)#2 + x(2)(1)#3 + 2 x(2)(3)#4
      + 6 x(2)(4)#5 + 5 x(3)(1)#6 + 2 x(3)(2)#7 + 3 x(3)(4)#8 + 4 x(4)(1)#9
      + 6 x(4)(2)#10 + 3 x(4)(3)#11
Subject To
 c1:  x(1)(2)#0 + x(1)(3)#1 + x(1)(4)#2 - x(2)(1)#3 - x(3)(1)#6 - x(4)(1)#9
       = 0
 c3:  - x(1)(2)#0 + x(2)(1)#3 + x(2)(3)#4 + x(2)(4)#5 - x(3)(2)#7 - x(4)(2)#10
       = 0
 c5:  - x(1)(3)#1 - x(2)(3)#4 + x(3)(1)#6 + x(3)(2)#7 + x(3)(4)#8 - x(4)(3)#11
       = 0
 c7:  - x(1)(4)#2 - x(2)(4)#5 - x(3)(4)#8 + x(4)(1)#9 + x(4)(2)#10 + x(4)(3)#11
       = 0
 c9:  x(1)(2)#0 + x(2)(1)#3 >= 1
 c11: x(1)(3)#1 + x(3)(1)#6 >= 1
 c13: x(1)(4)#2 + x(4)(1)#9 >= 1
 c15: x(2)(3)#4 + x(3)(2)#7 >= 1
 c17: x(2)(4)#5 + x(4)(2)#10 >= 1
 c19: x(3)(4)#8 + x(4)(3)#11 >= 1
Bounds
 0 <= x(1)(2)#0 <= 1
 0 <= x(1)(3)#1 <= 1
 0 <= x(1)(4)#2 <= 1
 0 <= x(2)(1)#3 <= 1
 0 <= x(2)(3)#4 <= 1
 0 <= x(2)(4)#5 <= 1
 0 <= x(3)(1)#6 <= 1
 0 <= x(3)(2)#7 <= 1
 0 <= x(3)(4)#8 <= 1
 0 <= x(4)(1)#9 <= 1
 0 <= x(4)(2)#10 <= 1
 0 <= x(4)(3)#11 <= 1
Generals
 x(1)(2)#0  x(1)(3)#1  x(1)(4)#2  x(2)(1)#3  x(2)(3)#4  x(2)(4)#5  x(3)(1)#6 
 x(3)(2)#7  x(3)(4)#8  x(4)(1)#9  x(4)(2)#10  x(4)(3)#11 
End
