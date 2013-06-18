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

