// RUN: %target-swift-frontend -parse-stdlib -emit-assembly %s -o - | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-cpu

// test converting floating point to unsigned integer
import Swift
let f = -123.0 as Float
let x = Builtin.fptoui_FPIEEE32_Int8(f._value)

// CHECK-NOT: (dummy CHECK line to avoid FileCheck complaints)
// CHECK-s390x: clfebr %r{{[0-9]+}}, {{[0-7]+}}, %f{{[0-9]+}}, {{[0-9]+}}

