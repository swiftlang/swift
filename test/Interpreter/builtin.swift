// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -parse-stdlib -Xfrontend -disable-access-control -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test

import Swift

print(String(Int32(Builtin.bitcast_FPIEEE32_Int32(Float32(1)._value)), radix: 16)) // CHECK: {{^}}3f800000{{$}}
print(String(UInt64(Builtin.bitcast_FPIEEE64_Int64(Float64(1)._value)), radix: 16)) // CHECK: {{^}}3ff0000000000000{{$}}

