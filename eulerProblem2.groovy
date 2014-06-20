def fib(previousTerm, currentTerm, total, max) {
    def nextTerm = currentTerm + previousTerm
    if (nextTerm >= max) {
        return total
    } else {
        return fib(currentTerm, nextTerm,
                nextTerm % 2 == 0 ? total + nextTerm : total, max)
    }

}
def answer= fib (0,1,0, 4000000)
println answer
assert answer==4613732