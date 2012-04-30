// RUN: %swift -I %S/.. %s -i | FileCheck %s

// CHECK: Ack(3,8)=2045

func ack(m : Int, n : Int) -> Int {
  if (m == 0) { return n+1 }
  if (n == 0) { return ack(m-1, 1) }
  return ack(m-1, ack(m, n-1))
}

println("Ack(3,8)=" + String(ack(3, 8)))

