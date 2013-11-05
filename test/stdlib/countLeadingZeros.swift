// RUN: %swift -i %s | FileCheck %s
// REQUIRES: swift_interpreter

def computeCountLeadingZeroes(x : Int) -> Int {
   var r = 64
   while (x != 0) {
      x >>= 1
      r--
   }
   return r
}

def testeCountLeadingZeroes() {
  for var i = 1; i < 1000; i++ {
     assert(countLeadingZeros(i) == computeCountLeadingZeroes(i))
  }
}

testeCountLeadingZeroes()
println("done!") // CHECK: {{^done!$}}
