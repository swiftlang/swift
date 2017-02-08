// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_catch %s | %FileCheck %s

enum SomeErr : Error {
  case Err1
  case Err2
}

struct S {
  // CHECK-LABEL: sil_coverage_map {{.*}}// coverage_catch.S.init
  init() {
    do {
      throw SomeErr.Err1
    } catch {
      // CHECK: [[@LINE-1]]:13 -> [[@LINE+1]]:6 : 2
    } // CHECK: [[@LINE]]:6 -> [[@LINE+1]]:4 : 1
  }
}

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_catch.bar
func bar() throws {
  // CHECK-NEXT: [[@LINE-1]]:19 -> [[@LINE+2]]:2 : 0
  throw SomeErr.Err2
} // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_catch.baz
func baz(_ fn: () throws -> ()) rethrows {
  do {
    try fn()
  } catch SomeErr.Err1 { // CHECK: [[@LINE]]:24 -> {{[0-9]+}}:4 : 2
    return
  } // CHECK-NEXT: [[@LINE]]:4 -> {{[0-9]+}}:2 : 1

  try fn()
} // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_catch.foo
func foo() -> Int32 {
  var x : Int32 = 0

  do {
    throw SomeErr.Err1
    x += 2 // [[@LINE]]:5 -> [[@LINE+1]]:4 : zero
  } catch SomeErr.Err1 {
    // CHECK: [[@LINE-1]]:24 -> [[@LINE+1]]:4 : 2
  } catch _ {
    // CHECK: [[@LINE-1]]:13 -> [[@LINE+1]]:4 : 3
  } // CHECK: [[@LINE]]:4 -> {{[0-9:]+}} : 1

  do {
    try baz(bar)
  } catch _ {
    // CHECK: [[@LINE-1]]:13 -> [[@LINE+1]]:4 : 5
  } // CHECK: [[@LINE]]:4 -> {{[0-9:]+}} : 4

  do {
    try baz { () throws -> () in throw SomeErr.Err1 }
  } catch _ {}

  try! baz { () throws -> () in return }

  return x
}

foo()
