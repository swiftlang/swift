// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xllvm -sil-disable-pass=GenericSpecializer %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test

public struct EmptyStruct {
}

public enum SingleCaseEnum {
  case A
}

public struct GenStruct<T> {
  var t: T
}

@inline(never)
func fail() {
  fatalError("size/stride mismatch")
}

@inline(never)
public func testit<T>(_ t: T) {
  if MemoryLayout<T>.size != 0 {
    fail()
  }
  if MemoryLayout<T>.stride != 1 {
    fail()
  }
  if MemoryLayout<GenStruct<T>>.size != 0 {
    fail()
  }
  if MemoryLayout<GenStruct<T>>.stride != 1 {
    fail()
  }
}

// Test size and stride which are loaded from the value witness tables.

testit(EmptyStruct())
testit(())
testit(SingleCaseEnum.A)

// Test size and stride which are computed as constants in IRGen.

if MemoryLayout<()>.size != 0 {
  fail()
}
if MemoryLayout<()>.stride != 1 {
  fail()
}

if MemoryLayout<EmptyStruct>.size != 0 {
  fail()
}
if MemoryLayout<EmptyStruct>.stride != 1 {
  fail()
}

if MemoryLayout<SingleCaseEnum>.size != 0 {
  fail()
}
if MemoryLayout<SingleCaseEnum>.stride != 1 {
  fail()
}

// CHECK: success
print("success")

