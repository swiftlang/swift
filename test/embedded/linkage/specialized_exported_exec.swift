// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -c -emit-module -o %t/Library.o %t/Library.swift -emit-module-path %t/Library.swiftmodule -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature ExtendedSpecializedAttribute -O
// RUN: %target-swift-frontend -c -o %t/Main.o %t/Main.swift -I %t -enable-experimental-feature Embedded -enable-experimental-feature ExtendedSpecializedAttribute -Onone
// RUN: %llvm-nm --defined-only %t/Main.o | %FileCheck -check-prefix MAIN %s
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Main.o %t/Library.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_ExtendedSpecializedAttribute

// The client doesn't define its own specialized body, and links with the
// library's.
// MAIN-NOT: $e7Library3sumyxx_xts17FixedWidthIntegerRzlFSi_

//--- Library.swift

@specialized(exported: true, where T == Int)
@inline(never)
public func sum<T: FixedWidthInteger>(_ a: T, _ b: T) -> T { a &+ b &* 3 }

//--- Main.swift

import Library

// CHECK: 8
print(sum(2, 2))
