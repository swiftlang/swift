// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_errors %s | %FileCheck %s
// RUN: %target-swift-frontend -profile-generate -profile-coverage-mapping -emit-ir %s

struct S {
  static subscript() -> Int {
    get throws { 5 }
  }
  subscript() -> Int {
    get throws { 5 }
  }
  func throwingMethod() throws -> Int { 0 }
}

func throwingFn() throws -> Int { 0 }

var throwingProp: Int {
  get throws { 5 }
}

var throwingS: S {
  get throws { S() }
}

enum SomeErr : Error {
  case Err1
  case Err2
}

// Unfortunately due the sorting of the SIL, we have to write the SIL function
// checks up here.

// func test1() -> Int {
//   do {
//     let x = try throwingFn()
//     return x
//   } catch {
//     return 0
//   }
// }
// CHECK-LABEL: sil hidden @$s15coverage_errors5test1SiyF : $@convention(thin) () -> Int
// CHECK:       bb0:
// CHECK:       increment_profiler_counter 0
// CHECK:       [[FN:%[0-9]+]] = function_ref @$s15coverage_errors10throwingFnSiyKF
// CHECK:       try_apply [[FN]]() : $@convention(thin) () -> (Int, @error any Error), normal [[BB_NORMAL:bb[0-9]+]], error [[BB_ERR:bb[0-9]+]]
//
// CHECK:       [[BB_ERR]]
// CHECK-NEXT:  increment_profiler_counter 1
//              FIXME: This next counter is redundant, we ought to be able to
//                     eliminate this with the SILOptimizer implementation.
// CHECK:       increment_profiler_counter 2

// func test2() throws -> Int {
//   let x = try throwingFn()
//   return x
// }
// CHECK-LABEL: sil hidden @$s15coverage_errors5test2SiyKF : $@convention(thin) () -> (Int, @error any Error)
// CHECK:       bb0:
// CHECK:       increment_profiler_counter 0
// CHECK:       [[FN:%[0-9]+]] = function_ref @$s15coverage_errors10throwingFnSiyKF
// CHECK:       try_apply [[FN]]() : $@convention(thin) () -> (Int, @error any Error), normal [[BB_NORMAL:bb[0-9]+]], error [[BB_ERR:bb[0-9]+]]
//
// CHECK:       [[BB_ERR]]
// CHECK-NEXT:  increment_profiler_counter 1

// func test3() throws -> Int {
//   let x = try throwingProp
//   return x
// }
// CHECK-LABEL: sil hidden @$s15coverage_errors5test3SiyKF : $@convention(thin) () -> (Int, @error any Error)
// CHECK:       bb0:
// CHECK:       increment_profiler_counter 0
// CHECK:       [[FN:%[0-9]+]] = function_ref @$s15coverage_errors12throwingPropSivg
// CHECK:       try_apply [[FN]]() : $@convention(thin) () -> (Int, @error any Error), normal [[BB_NORMAL:bb[0-9]+]], error [[BB_ERR:bb[0-9]+]]
//
// CHECK:       [[BB_ERR]]
// CHECK-NEXT:  increment_profiler_counter 1

// func test4() throws -> Int {
//   let x = try S[]
//   return x
// }
// CHECK-LABEL: sil hidden @$s15coverage_errors5test4SiyKF : $@convention(thin) () -> (Int, @error any Error)
// CHECK:       bb0:
// CHECK:       increment_profiler_counter 0
// CHECK:       [[FN:%[0-9]+]] = function_ref @$s15coverage_errors1SVSiycigZ
// CHECK:       try_apply [[FN]]({{%[0-9]+}}) : $@convention(method) (@thin S.Type) -> (Int, @error any Error), normal [[BB_NORMAL:bb[0-9]+]], error [[BB_ERR:bb[0-9]+]]
//
// CHECK:       [[BB_ERR]]
// CHECK-NEXT:  increment_profiler_counter 1

// func test17(
//   _ x: S
// ) throws -> Int {
//   let y = try x[]
//   return y
// }
// CHECK-LABEL: sil hidden @$s15coverage_errors6test17ySiAA1SVKF : $@convention(thin) (S) -> (Int, @error any Error)
// CHECK:       bb0(%0 : $S):
// CHECK:       increment_profiler_counter 0
// CHECK:       [[FN:%[0-9]+]] = function_ref @$s15coverage_errors1SVSiycig
// CHECK:       try_apply [[FN]](%0)  : $@convention(method) (S) -> (Int, @error any Error), normal [[BB_NORMAL:bb[0-9]+]], error [[BB_ERR:bb[0-9]+]]
//
// CHECK:       [[BB_ERR]]
// CHECK-NEXT:  increment_profiler_counter 1

// func test18(
//   _ x: S
// ) throws -> Int {
//   let y = try x.throwingMethod()
//   return y
// }
// CHECK-LABEL: sil hidden @$s15coverage_errors6test18ySiAA1SVKF : $@convention(thin) (S) -> (Int, @error any Error)
// CHECK:       bb0(%0 : $S):
// CHECK:       increment_profiler_counter 0
// CHECK:       [[FN:%[0-9]+]] = function_ref @$s15coverage_errors1SV14throwingMethodSiyKF
// CHECK:       try_apply [[FN]](%0) : $@convention(method) (S) -> (Int, @error any Error), normal [[BB_NORMAL:bb[0-9]+]], error [[BB_ERR:bb[0-9]+]]
//
// CHECK:       [[BB_ERR]]
// CHECK-NEXT:  increment_profiler_counter 1

// func test19() throws -> Int {
//   let x = try throwingS.throwingMethod()
//   return x
// }
// CHECK-LABEL: sil hidden @$s15coverage_errors6test19SiyKF : $@convention(thin) () -> (Int, @error any Error)
// CHECK:       bb0:
// CHECK:       increment_profiler_counter 0
// CHECK:       [[GETTER:%[0-9]+]] = function_ref @$s15coverage_errors9throwingSAA1SVvg
// CHECK:       try_apply [[GETTER]]() : $@convention(thin) () -> (S, @error any Error), normal [[BB_NORMAL:bb[0-9]+]], error [[BB_ERR:bb[0-9]+]]
//
// CHECK:       [[BB_ERR]]
// CHECK-NEXT:  increment_profiler_counter 1
// CHECK:       [[BB_NORMAL]]([[S:%[0-9]+]] : $S):
// CHECK:       [[FN:%[0-9]+]] = function_ref @$s15coverage_errors1SV14throwingMethodSiyKF
// CHECK:       try_apply [[FN]]([[S]]) : $@convention(method) (S) -> (Int, @error any Error), normal [[BB_NORMAL:bb[0-9]+]], error [[BB_ERR:bb[0-9]+]]
//
// CHECK:       [[BB_ERR]]
// CHECK-NEXT:  increment_profiler_counter 2

// func test21() throws -> Int {
//   try { throw SomeErr.Err1 }()
//   return 1
// }
// CHECK-LABEL: sil hidden @$s15coverage_errors6test21SiyKF : $@convention(thin) () -> (Int, @error any Error)
// CHECK:       bb0:
// CHECK:       increment_profiler_counter 0
// CHECK:       [[FN:%[0-9]+]] = function_ref @$s15coverage_errors6test21SiyKFyyKXEfU_
// CHECK:       try_apply [[FN]]() : $@convention(thin) () -> @error any Error, normal [[BB_NORMAL:bb[0-9]+]], error [[BB_ERR:bb[0-9]+]]
//
// CHECK:       [[BB_ERR]]
// CHECK-NEXT:  increment_profiler_counter 1

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors5test1SiyF"
func test1() -> Int {        // CHECK-NEXT: [[@LINE]]:21 -> [[@LINE+7]]:2  : 0
  do {                       // CHECK-NEXT: [[@LINE]]:6  -> [[@LINE+3]]:4  : 0
    let x = try throwingFn() // CHECK-NEXT: [[@LINE]]:29 -> [[@LINE+1]]:13 : (0 - 1)
    return x
  } catch {                  // CHECK-NEXT: [[@LINE]]:11 -> [[@LINE+2]]:4 : 2
    return 0
  }
}                            // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors5test2SiyKF"
func test2() throws -> Int { // CHECK-NEXT: [[@LINE]]:28 -> [[@LINE+3]]:2  : 0
  let x = try throwingFn()   // CHECK-NEXT: [[@LINE]]:27 -> [[@LINE+1]]:11 : (0 - 1)
  return x
}                            // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors5test3SiyKF"
func test3() throws -> Int { // CHECK-NEXT: [[@LINE]]:28 -> [[@LINE+3]]:2  : 0
  let x = try throwingProp   // CHECK-NEXT: [[@LINE]]:27 -> [[@LINE+1]]:11 : (0 - 1)
  return x
}                            // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors5test4SiyKF"
func test4() throws -> Int { // CHECK-NEXT: [[@LINE]]:28 -> [[@LINE+3]]:2  : 0
  let x = try S[]            // CHECK-NEXT: [[@LINE]]:18 -> [[@LINE+1]]:11 : (0 - 1)
  return x
}                            // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors5test5yyKF"
func test5() throws {
  // CHECK-NEXT: [[@LINE-1]]:21 -> [[@LINE+2]]:2 : 0
  throw SomeErr.Err2
} // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors5test6yyyyKXEKF"
func test6(
  _ fn: () throws -> ()
) rethrows {             // CHECK-NEXT: [[@LINE]]:12 -> [[@LINE+8]]:2 : 0
  do {                   // CHECK-NEXT: [[@LINE]]:6  -> [[@LINE+2]]:4 : 0
    try fn()             // CHECK-NEXT: [[@LINE]]:13 -> [[@LINE+1]]:4 : (0 - 1)
  } catch SomeErr.Err1 { // CHECK-NEXT: [[@LINE]]:24 -> [[@LINE+2]]:4 : 2
    return
  }                      // CHECK-NEXT: [[@LINE]]:4  -> [[@LINE+3]]:2 : (0 - 1)

  try fn()               // CHECK-NEXT: [[@LINE]]:11 -> [[@LINE+1]]:2 : ((0 - 1) - 3)
}                        // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors5test7s5Int32VyF"
func test7() -> Int32 {  // CHECK-NEXT: [[@LINE]]:23 -> [[@LINE+31]]:2 : 0
  var x : Int32 = 0

  do {                   // CHECK-NEXT: [[@LINE]]:6  -> [[@LINE+3]]:4 : 0
    throw SomeErr.Err1
    x += 2               // CHECK-NEXT: [[@LINE]]:5  -> [[@LINE+1]]:4 : zero
  } catch SomeErr.Err1 { // CHECK-NEXT: [[@LINE]]:24 -> [[@LINE+1]]:4 : 1
  } catch _ {            // CHECK-NEXT: [[@LINE]]:13 -> [[@LINE+1]]:4 : 2
  }                      // CHECK-NEXT: [[@LINE]]:4  -> {{[0-9:]+}}   : (1 + 2)

  do {                   // CHECK-NEXT: [[@LINE]]:6  -> [[@LINE+2]]:4 : (1 + 2)
    try test6(test5)     // CHECK-NEXT: [[@LINE]]:21 -> [[@LINE+1]]:4 : ((1 + 2) - 3)
  } catch _ {            // CHECK-NEXT: [[@LINE]]:13 -> [[@LINE+1]]:4 : 4
  }                      // CHECK-NEXT: [[@LINE]]:4  -> {{[0-9:]+}}   : (((1 + 2) + 4) - 3)

  do {                   // CHECK-NEXT: [[@LINE]]:6  -> [[@LINE+5]]:4 : (((1 + 2) + 4) - 3)
    try test6 {          // (closures are mapped separately)
      () throws -> () in
      throw SomeErr.Err1
    }                    // CHECK-NEXT: [[@LINE]]:6  -> [[@LINE+1]]:4 : ((((1 + 2) + 4) - 3) - 5)
  } catch _ {}           // CHECK-NEXT: [[@LINE]]:13 -> [[@LINE]]:15  : 6
                         // CHECK-NEXT: [[@LINE-1]]:15 -> {{[0-9:]+}} : (((((1 + 2) + 4) + 6) - 3) - 5)

  // TODO: We ought to realize that everything after try! is unreachable
  // This is similar issue to rdar://100896177
  try! test6 {
    () throws -> () in
    return
  }                      // CHECK-NEXT: [[@LINE]]:4  -> [[@LINE+2]]:11 : ((((((1 + 2) + 4) + 6) - 3) - 5) - 7

  return x
}                        // CHECK-NEXT: }

// rdar://34244637 - Coverage after a do-catch is incorrect
// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors5test8ySiSbF"
func test8(_ b: Bool) -> Int { // CHECK-NEXT: [[@LINE]]:30 {{.*}} : 0
  do {                         // CHECK-NEXT: [[@LINE]]:6 -> [[@LINE+2]]:4 : 0
    throw SomeErr.Err1
  } catch {                    // CHECK-NEXT: [[@LINE]]:11 {{.*}} : 1
                               // CHECK-NEXT: [[@LINE+1]]:8 {{.*}} : 1
    if b {                     // CHECK-NEXT: [[@LINE]]:10 {{.*}} : 2
      return 1
    }                          // CHECK-NEXT: [[@LINE]]:6 {{.*}} : (1 - 2)
  }                            // CHECK: [[@LINE]]:4 {{.*}} : (1 - 2)
  return 0
}

// Test coverage with nested do-catches
// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors5test9SiyF"
func test9() -> Int {    // CHECK-NEXT: [[@LINE]]:21 -> [[@LINE+11]]:2 : 0
  do {                   // CHECK-NEXT: [[@LINE]]:6  -> [[@LINE+7]]:4 : 0
    try test5()          // CHECK-NEXT: [[@LINE]]:16 -> [[@LINE+6]]:4 : (0 - 1)
    do {                 // CHECK-NEXT: [[@LINE]]:8  -> [[@LINE+2]]:6 : (0 - 1)
      throw SomeErr.Err1
    } catch {            // CHECK-NEXT: [[@LINE]]:13 -> [[@LINE+2]]:6 : 2
      return 0
    }
  } catch {              // CHECK-NEXT: [[@LINE]]:11 -> [[@LINE+2]]:4 : 3
    return 1
  }
}                        // CHECK-NEXT: }

// Test coverage with a do-catch inside of a repeat-while
// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test10SiyF"
func test10() -> Int {
  repeat {               // CHECK: [[@LINE]]:10 {{.*}} : 1
    do {
      throw SomeErr.Err1
    } catch {            // CHECK: [[@LINE]]:13 {{.*}} : 2
      return 0
    }                    // CHECK-NOT: [[@LINE]]:6 ->

  } while false          // CHECK: [[@LINE]]:11 {{.*}} : (1 - 2)
  return 1
}

// Test coverage with a break inside a do-catch inside of a repeat-while
// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test11SiyF"
func test11() -> Int { // CHECK-NEXT: [[@LINE]]:22 -> [[@LINE+11]]:2 : 0
  repeat {             // CHECK-NEXT: [[@LINE]]:10 -> [[@LINE+8]]:4 : 1
    do {               // CHECK-NEXT: [[@LINE]]:8  -> [[@LINE+2]]:6 : 1
      try test5()      // CHECK-NEXT: [[@LINE]]:18 -> [[@LINE+1]]:6 : (1 - 2)
    } catch {          // CHECK-NEXT: [[@LINE]]:13 -> [[@LINE+2]]:6 : 3
      break
    }                  // CHECK-NEXT: [[@LINE]]:6   -> [[@LINE+3]]:4  : (1 - 2)
                       // FIXME: This exit counter is wrong (rdar://118472537)
                       // CHECK-NEXT: [[@LINE+1]]:4 -> [[@LINE+2]]:11 : (0 - 2)
  } while false        // CHECK-NEXT: [[@LINE]]:11  -> [[@LINE]]:16   : (1 - 3)
  return 1
}                      // CHECK-NEXT: }

// rdar://41010883 – Make sure we don't introduce an empty unreachable region.
// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test12SiyKF"
func test12() throws -> Int { // CHECK-NEXT: [[@LINE]]:29 -> [[@LINE+7]]:2  : 0
  do {                        // CHECK-NEXT: [[@LINE]]:6  -> [[@LINE+3]]:4  : 0
    try test5()               // CHECK-NEXT: [[@LINE]]:16 -> [[@LINE+1]]:13 : (0 - 1)
    return 1
  } catch is SomeErr {        // CHECK-NEXT: [[@LINE]]:22 -> [[@LINE+2]]:4 : 2
    throw SomeErr.Err1
  }
}                             // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test13SiyF"
func test13() -> Int {      // CHECK-NEXT: [[@LINE]]:22 -> [[@LINE+6]]:2 : 0
  do {                      // CHECK-NEXT: [[@LINE]]:6  -> [[@LINE+2]]:4 : 0
    return try throwingFn() // Note we don't emit a region here because it would be empty.
  } catch {                 // CHECK-NEXT: [[@LINE]]:11 -> [[@LINE+2]]:4 : 2
    return 0
  }
}                           // CHECK-NEXT: }

func takesInts(_ x: Int, _ y: Int) {}

// The throwing expr is nested here, the region starts after the throwing expr.
// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test14yyKF"
func test14() throws {           // CHECK-NEXT: [[@LINE]]:22 -> [[@LINE+2]]:2 : 0
  takesInts(try throwingFn(), 0) // CHECK-NEXT: [[@LINE]]:29 -> [[@LINE+1]]:2 : (0 - 1)
}                                // CHECK-NEXT: }

// The return can be reached via the catch if SomeErr.Err1 was thrown, OR the
// 'do' block if an error wasn't thrown.
// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test15SiyKF"
func test15() throws -> Int { // CHECK-NEXT: [[@LINE]]:29 -> [[@LINE+6]]:2  : 0
  do {                        // CHECK-NEXT: [[@LINE]]:6  -> [[@LINE+2]]:4  : 0
    try test5()               // CHECK-NEXT: [[@LINE]]:16 -> [[@LINE+1]]:4  : (0 - 1)
  } catch SomeErr.Err1 {      // CHECK-NEXT: [[@LINE]]:24 -> [[@LINE+1]]:4  : 2
  }                           // CHECK-NEXT: [[@LINE]]:4  -> [[@LINE+1]]:11 : ((0 + 2) - 1)
  return 2
}                             // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test16SiyKF"
func test16() throws -> Int { // CHECK-NEXT: [[@LINE]]:29 -> [[@LINE+9]]:2  : 0
  do {                        // CHECK-NEXT: [[@LINE]]:6  -> [[@LINE+5]]:4  : 0
    do {                      // CHECK-NEXT: [[@LINE]]:8  -> [[@LINE+2]]:6  : 0
      try test5()             // CHECK-NEXT: [[@LINE]]:18 -> [[@LINE+1]]:6  : (0 - 1)
    } catch SomeErr.Err1 {    // CHECK-NEXT: [[@LINE]]:26 -> [[@LINE+1]]:6  : 2
    }                         // CHECK-NEXT: [[@LINE]]:6  -> [[@LINE+1]]:4  : ((0 + 2) - 1)
  } catch SomeErr.Err2 {      // CHECK-NEXT: [[@LINE]]:24 -> [[@LINE+1]]:4  : 3
  }                           // CHECK-NEXT: [[@LINE]]:4  -> [[@LINE+1]]:11 : (((0 + 2) + 3) - 1)
  return 2
}                             // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test17ySiAA1SVKF"
func test17(
  _ x: S
) throws -> Int { // CHECK-NEXT: [[@LINE]]:17 -> [[@LINE+3]]:2  : 0
  let y = try x[] // CHECK-NEXT: [[@LINE]]:18 -> [[@LINE+1]]:11 : (0 - 1)
  return y
}                 // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test18ySiAA1SVKF"
func test18(
  _ x: S
) throws -> Int {                // CHECK-NEXT: [[@LINE]]:17 -> [[@LINE+3]]:2  : 0
  let y = try x.throwingMethod() // CHECK-NEXT: [[@LINE]]:33 -> [[@LINE+1]]:11 : (0 - 1)
  return y
}                                // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test19SiyKF"
func test19() throws -> Int {            // CHECK-NEXT: [[@LINE]]:29 -> [[@LINE+3]]:2  : 0
  let x = try throwingS.throwingMethod() // CHECK-NEXT: [[@LINE]]:24 -> [[@LINE+1]]:11 : (0 - 1)
  return x                               // CHECK-NEXT: [[@LINE-1]]:41 -> [[@LINE]]:11 : ((0 - 1) - 2)
}                                        // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test20yyKF"
func test20() throws { // CHECK-NEXT: [[@LINE]]:22 -> [[@LINE+5]]:2 : 0
  takesInts(
    try throwingFn(),  // CHECK-NEXT: [[@LINE]]:21 -> [[@LINE+3]]:2 : (0 - 1)
    try throwingFn()   // CHECK-NEXT: [[@LINE]]:21 -> [[@LINE+2]]:2 : ((0 - 1) - 2)
  )
}                      // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test21SiyKF"
func test21() throws -> Int {  // CHECK-NEXT: [[@LINE]]:29 -> [[@LINE+3]]:2 : 0
  try { throw SomeErr.Err1 }() // CHECK-NEXT: [[@LINE]]:31 -> [[@LINE+1]]:11 : (0 - 1)
  return 1
}                              // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test22SiyKF"
func test22() throws -> Int { // CHECK-NEXT: [[@LINE]]:29 -> [[@LINE+7]]:2 : 0
  x: do {                     // CHECK-NEXT: [[@LINE]]:9  -> [[@LINE+2]]:4 : 0
    try test5()               // CHECK-NEXT: [[@LINE]]:16 -> [[@LINE+1]]:4 : (0 - 1)
  } catch SomeErr.Err1 {      // CHECK-NEXT: [[@LINE]]:24 -> [[@LINE+2]]:4 : 2
    break x
  }                           // CHECK-NEXT: [[@LINE]]:4 -> [[@LINE+1]]:11 : ((0 + 2) - 1)
  return 1                    // CHECK-NEXT: }
}

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test23SiyKF"
func test23() throws -> Int { // CHECK-NEXT: [[@LINE]]:29 -> [[@LINE+11]]:2 : 0
  x: do {                     // CHECK-NEXT: [[@LINE]]:9  -> [[@LINE+7]]:4  : 0
    do {                      // CHECK-NEXT: [[@LINE]]:8  -> [[@LINE+3]]:6  : 0
      try test5()             // CHECK-NEXT: [[@LINE]]:18 -> [[@LINE+1]]:14 : (0 - 1)
      break x
    } catch SomeErr.Err1 {    // CHECK-NEXT: [[@LINE]]:26 -> [[@LINE+1]]:6  : 2
    }                         // CHECK-NEXT: [[@LINE]]:6  -> [[@LINE+1]]:13 : 2
    return 1
  } catch SomeErr.Err2 {      // CHECK-NEXT: [[@LINE]]:24 -> [[@LINE+1]]:4  : 3
  }                           // CHECK-NEXT: [[@LINE]]:4  -> [[@LINE+1]]:11 : ((0 + 3) - 1)
  return 2
}                             // CHECK-NEXT: }

struct TestInit {
  // CHECK-LABEL: sil_coverage_map {{.*}}// coverage_errors.TestInit.init() -> coverage_errors.TestInit
  init() {               // CHECK-NEXT: [[@LINE]]:10 -> [[@LINE+5]]:4 : 0
    do {                 // CHECK-NEXT: [[@LINE]]:8  -> [[@LINE+2]]:6 : 0
      throw SomeErr.Err1
    } catch {            // CHECK-NEXT: [[@LINE]]:13 -> [[@LINE+1]]:6 : 1
    }                    // CHECK-NEXT: [[@LINE]]:6  -> [[@LINE+1]]:4 : 1
  }                      // CHECK-NEXT: }
}
