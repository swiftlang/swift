// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift \
// RUN:     -emit-module \
// RUN:     -target %target-swift-5.9-abi-triple \
// RUN:     %t/Library.swift \
// RUN:     -parse-as-library \
// RUN:     -module-name Library \
// RUN:     -emit-module-path %t/Library.swiftmodule

// RUN: %target-swift-frontend                              \
// RUN:     %t/Downstream.swift                             \
// RUN:     -emit-irgen                                     \
// RUN:     -target %target-swift-5.9-abi-triple            \
// RUN:     -module-name main                               \
// RUN:     -lLibrary                                       \
// RUN:     -I %t                                           \
// RUN: | %FileCheck %t/Downstream.swift --check-prefixes=CHECK,CHECK-OLD

//--- Library.swift

public struct Pack<each T> {
  public init() {}
}

public func sink<T>(_ t: T) {}

//--- Downstream.swift

import Library

// CHECK: doit
// CHECK: @"$s7Library4PackVMa"{{.*}} [[CALL:#[^,]+]]

// CHECK: attributes [[CALL]] = { nounwind memory(argmem: readwrite, inaccessiblemem: readwrite) }
@_silgen_name("doit")
func doit<each T, each U>(ts: repeat each T, us: repeat each U) {
  sink(Pack<repeat each T, repeat each U>())
}
