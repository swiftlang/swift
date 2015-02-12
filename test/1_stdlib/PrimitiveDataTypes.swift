// RUN: %target-run-simple-swift | FileCheck %s

//====  Tests for UnicodeScalar ====

var CharA: UnicodeScalar = "A" // 65 in decimal.

println(UInt8(ascii: CharA)) // CHECK: {{^65$}}
println(UInt32(CharA)) // CHECK: {{^65$}}
println(UInt64(CharA)) // CHECK: {{^65$}}

if CharA == "A" && "A" == CharA &&
   CharA != "B" && "B" != CharA {
  println("equality comparison works") // CHECK: equality comparison works
}

if CharA < "B" && "B" > CharA &&
   CharA <= "B" && "B" >= CharA {
  println("relational comparison works") // CHECK: relational comparison works
}

println(UnicodeScalar(0).isASCII()) // CHECK: true
println(CharA.isASCII()) // CHECK: true
println(UnicodeScalar(127).isASCII()) // CHECK: true
println(UnicodeScalar(128).isASCII()) // CHECK: false
println(UnicodeScalar(256).isASCII()) // CHECK: false

//====  Tests for Bool ====

func truthy() -> Bool {
  print("truthy ")
  return true
}

func falsy() -> Bool {
  print("falsy ")
  return false
}

func boolTests() {
  println("\(!true)") // CHECK: false
  println("\(!false)") // CHECK: true

  // Test short-circuiting operators
  println("\(truthy() && truthy())") // CHECK: truthy truthy true
  println("\(truthy() && falsy())") // CHECK: truthy falsy false
  println("\(falsy() && truthy())") // CHECK: falsy false
  println("\(falsy() && falsy())") // CHECK: falsy false

  println("\(truthy() || truthy())") // CHECK: truthy true
  println("\(truthy() || falsy())") // CHECK: truthy true
  println("\(falsy() || truthy())") // CHECK: falsy truthy true
  println("\(falsy() || falsy())") // CHECK: falsy falsy false
}

boolTests()
