// RUN: %swift -i %s | FileCheck %s

var CharA = 'A' // 65 in decimal.

println(UInt8(CharA)) // CHECK: {{^65$}}
println(UInt32(CharA)) // CHECK: {{^65$}}
println(UInt64(CharA)) // CHECK: {{^65$}}

println(CharA - 'B') // CHECK: {{^-1$}}
println('B' - CharA) // CHECK: {{^1$}}

println((CharA - 2).value) // CHECK {{^63$}}
println((CharA - -2).value) // CHECK {{^67$}}

println((CharA + 3).value) // CHECK: {{^68$}}
println((4 + CharA).value) // CHECK: {{^69$}}

if CharA == 'A' && 'A' == CharA &&
   CharA != 'B' && 'B' != CharA {
  println("equality comparison works") // CHECK: equality comparison works
}

if CharA < 'B' && 'B' > CharA &&
   CharA <= 'B' && 'B' >= CharA {
  println("relational comparison works") // CHECK: relational comparison works
}

println(Char(0).isASCII()) // CHECK: true
println(CharA.isASCII()) // CHECK: true
println(Char(127).isASCII()) // CHECK: true
println(Char(128).isASCII()) // CHECK: false
println(Char(256).isASCII()) // CHECK: false

