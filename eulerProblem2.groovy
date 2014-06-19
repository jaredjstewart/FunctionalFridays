List<Integer> sumOfEvenFibsLessThanMaximum (Integer firstTerm, Integer secondTerm, Integer maximum) {
    def fibs = [firstTerm, secondTerm]
    def nextTerm = firstTerm + secondTerm
    while (nextTerm <maximum) 
     {
     fibs << nextTerm
     nextTerm = fibs[-1] + fibs[-2]
     }
     return fibs.findAll(it % 2 == 0).sum()
}    

assert 4613732 == sumOfEvenFibsLessThanMaximum(0,1, 4000000)


     