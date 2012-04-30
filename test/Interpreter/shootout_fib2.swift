// RUN: %swift -I %S/.. %s -i | FileCheck %s

// CHECK: 10946

func fib(n : Int) -> Int {
  if (n < 2) {
    return 1
  }
  
  return fib(n-2) + fib(n-1)
}

println(fib(20))
