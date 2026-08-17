// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines
// RUN: %target-build-swift %t/main.swift -O -o %t/a.out -import-objc-header %t/header.h
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

// The ObjectOutliner creates statically initialized arrays for the literals
// below. The element types contain trailing padding, which must be taken into
// account when IRGen emits the static initializer - otherwise the array
// elements end up at wrong offsets.
// https://github.com/swiftlang/swift/issues/91387

//--- header.h

struct CStructWithPadding {
  long long a;
  int b;
  // 4 bytes of trailing padding on 64 bit platforms
};

//--- main.swift

let structArray = [CStructWithPadding(), CStructWithPadding(a: 1, b: 2), CStructWithPadding(a: 3, b: 4)]

// CHECK: [0, 0, 1, 2, 3, 4]
print(structArray.flatMap { [Int($0.a), Int($0.b)] })

let tupleArray: [(Int, CStructWithPadding)] = [(1, CStructWithPadding()), (2, CStructWithPadding()), (3, CStructWithPadding())]

// CHECK: [1, 2, 3]
print(tupleArray.map { $0.0 })

let dict: [Int: CStructWithPadding] = [1: CStructWithPadding(), 2: CStructWithPadding(), 3: CStructWithPadding()]

// CHECK: 3
print(dict.count)

// CHECK: [1, 2, 3]
print(dict.keys.sorted())
