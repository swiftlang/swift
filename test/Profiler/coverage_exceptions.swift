// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_catch %s | %FileCheck %s

enum SomeErr : Error {
  case Err1
  case Err2
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
  } catch SomeErr.Err1 { // CHECK: [[@LINE]]:24 -> {{[0-9]+}}:4 : 1
    return
  } // CHECK-NEXT: [[@LINE]]:4 -> {{[0-9]+}}:2 : (0 - 1)

  try fn()
} // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_catch.foo
func foo() -> Int32 {
  var x : Int32 = 0

  do {
    throw SomeErr.Err1
    x += 2 // [[@LINE]]:5 -> [[@LINE+1]]:4 : zero
  } catch SomeErr.Err1 {
    // CHECK: [[@LINE-1]]:24 -> [[@LINE+1]]:4 : 1
  } catch _ {
    // CHECK: [[@LINE-1]]:13 -> [[@LINE+1]]:4 : 2
  } // CHECK: [[@LINE]]:4 -> {{[0-9:]+}} : 0

  do {
    try baz(bar)
  } catch _ {
    // CHECK: [[@LINE-1]]:13 -> [[@LINE+1]]:4 : 3
  } // CHECK: [[@LINE]]:4 -> {{[0-9:]+}} : 0

  do {
    try baz { () throws -> () in throw SomeErr.Err1 }
  } catch _ {}

  try! baz { () throws -> () in return }

  return x
}

let _ = foo()

// rdar://34244637 - Coverage after a do-catch is incorrect
// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_catch.goo
func goo(_ b: Bool) -> Int { // CHECK-NEXT: [[@LINE]]:28 {{.*}} : 0
  do {                       // CHECK-NEXT: [[@LINE]]:6 -> [[@LINE+2]]:4 : 0
    throw SomeErr.Err1
  } catch {                  // CHECK-NEXT: [[@LINE]]:11 {{.*}} : 1
    if b {                   // CHECK-NEXT: [[@LINE]]:10 {{.*}} : 2
      return 1
    }                        // CHECK-NEXT: [[@LINE]]:6 {{.*}} : (1 - 2)
  } // CHECK: [[@LINE]]:4 {{.*}} : (0 - 2)
  return 0
}

// Test coverage with nested do-catches
// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_catch.hoo
func hoo() -> Int {
  do {
    try bar()
    do {
      throw SomeErr.Err1
    } catch {
      return 0
    } // CHECK: [[@LINE]]:6 {{.*}} : (0 - 1)

  } catch {
    return 1
  } // CHECK: [[@LINE]]:4 {{.*}} : ((0 - 1) - 2)

}

// Test coverage with a do-catch inside of a repeat-while
// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_catch.ioo
func ioo() -> Int {
  repeat { // CHECK: [[@LINE]]:10 {{.*}} : 1
    do {
      throw SomeErr.Err1
    } catch { // CHECK: [[@LINE]]:13 {{.*}} : 2
      return 0
    } // CHECK: [[@LINE]]:6 {{.*}} : (1 - 2)

  } while false // CHECK: [[@LINE]]:11 {{.*}} : (1 - 2)
  return 1
}

// Test coverage with a break inside a do-catch inside of a repeat-while
// CHECK-LABEL: sil_coverage_map {{.*}}// coverage_catch.joo
func joo() -> Int {
  repeat { // CHECK: [[@LINE]]:10 {{.*}} : 1
    do {
      try bar()
    } catch { // CHECK: [[@LINE]]:13 {{.*}} : 2
      break
    } // CHECK: [[@LINE]]:6 {{.*}} : (1 - 2)

  } while false // CHECK: [[@LINE]]:11 {{.*}} : (1 - 2)
  return 1
}

struct S {
  // CHECK: sil_coverage_map {{.*}}// __ntd_S_line:[[@LINE-1]]
  init() {
    do {
      throw SomeErr.Err1
    } catch {
      // CHECK: [[@LINE-1]]:13 -> [[@LINE+1]]:6 : 1
    } // CHECK: [[@LINE]]:6 -> [[@LINE+1]]:4 : 0
  }
}
