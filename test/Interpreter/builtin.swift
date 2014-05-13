// RUN: %target-run-stdlib-swift | FileCheck %s

import Swift

println(_uint64ToString(UInt64(Int32(Builtin.bitcast_FPIEEE32_Int32(Float32(1).value))), radix: 16)) // CHECK: {{^}}3f800000{{$}}
println(_uint64ToString(UInt64(Builtin.bitcast_FPIEEE64_Int64(Float64(1).value)), radix: 16)) // CHECK: {{^}}3ff0000000000000{{$}}

// Check that floating point literals work
println("\(1.0)") // CHECK: {{^}}1{{$}}
println("\(-1.0)") // CHECK: {{^}}-1{{$}}
println("\(Float32(340282350000000000000000000000000000000.0))") // CHECK: {{^}}3.40282346638529e+38{{$}}
println("\(Float32(-340282350000000000000000000000000000000.0))") // CHECK: {{^}}-3.40282346638529e+38{{$}}
let double : Double = 0.999999999
println(double) // CHECK: {{^}}0.999999999{{$}}

println("\(680564733841876926926749214863536422912.0)") // CHECK: {{^}}6.80564733841877e+38{{$}}
println("\(-680564733841876926926749214863536422912.0)") // CHECK: {{^}}-6.80564733841877e+38{{$}}

// Check that floating point numbers can be constructed from large integer
// literals (that have more than 64 bits).
var f0 : Float32 = 340282350000000000000000000000000000000
println("\(f0)") // CHECK: {{^}}3.40282346638529e+38{{$}}
var f1 : Float32 = -340282350000000000000000000000000000000
println("\(f1)") // CHECK: {{^}}-3.40282346638529e+38{{$}}

var f2 : Float64 = 680564733841876926926749214863536422912
println("\(f2)") // CHECK: {{^}}6.80564733841877e+38{{$}}
var f3 : Float64 = -680564733841876926926749214863536422912
println("\(f3)") // CHECK: {{^}}-6.80564733841877e+38{{$}}

