// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -profile-generate %S/Inputs/profiler_serialized.swift -module-name Foo -o %t/Foo.swiftmodule
// RUN: %target-swift-frontend -emit-sil -profile-generate %t/Foo.swiftmodule | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil -profile-generate -module-name profiler_serialized %s -I %t | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -profile-generate %s -I %t

import Foo

// CHECK-LABEL: sil shared @$s3Foo19functionToSerializeSiyF
// CHECK:       increment_profiler_counter 0, "$s3Foo19functionToSerializeSiyF", num_counters 2, hash 0
// CHECK:       increment_profiler_counter 1, "$s3Foo19functionToSerializeSiyF", num_counters 2, hash 0

public func bar() -> Int {
  functionToSerialize()
}
