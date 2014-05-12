// RUN: %target-run-simple-swift | FileCheck %s

func computeCountLeadingZeroes(xi: Int64) -> Int64 {
   var r : Int64 = 64
   var x = xi
   while (x != 0) {
      x >>= 1
      r--
   }
   return r
}

func testeCountLeadingZeroes() {
  for var i : Int64 = 1; i < 1000; i++ {
     assert(countLeadingZeros(i) == computeCountLeadingZeroes(i))
  }
}

testeCountLeadingZeroes()
println("done!") // CHECK: {{^done!$}}
