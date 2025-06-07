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

@discardableResult
func throwingFn() throws -> Int { 0 }

func throwingBool() throws -> Bool { true }

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

// func test28() throws -> Int {
//   let x = try .random()
//     ? throwingFn()
//     : throwingFn()
//   return x
// }
// CHECK-LABEL: sil hidden @$s15coverage_errors6test28SiyKF : $@convention(thin) () -> (Int, @error any Error)
// CHECK:       bb0:
// CHECK:       increment_profiler_counter 0
// CHECK:       function_ref @$sSb6randomSbyFZ
// CHECK:       cond_br {{%[0-9]+}}, [[BB_TRUE:bb[0-9]+]], [[BB_FALSE:bb[0-9]+]]
//
// CHECK:       [[BB_FALSE]]
// CHECK:       [[THROW_FN:%[0-9]+]] = function_ref @$s15coverage_errors10throwingFnSiyKF
// CHECK:       try_apply [[THROW_FN]]() : $@convention(thin) () -> (Int, @error any Error), normal [[BB_NORMAL:bb[0-9]+]], error [[BB_ERR:bb[0-9]+]]
//
// CHECK:       [[BB_ERR]]
// CHECK:       increment_profiler_counter 3
//
// CHECK:       [[BB_TRUE]]
// CHECK:       increment_profiler_counter 1
// CHECK:       [[THROW_FN:%[0-9]+]] = function_ref @$s15coverage_errors10throwingFnSiyKF
// CHECK:       try_apply [[THROW_FN]]() : $@convention(thin) () -> (Int, @error any Error), normal [[BB_NORMAL:bb[0-9]+]], error [[BB_ERR:bb[0-9]+]]
//
// CHECK:       [[BB_ERR]]
// CHECK:       increment_profiler_counter 2

// func test43() -> Int? {
//   let x = try? throwingS.throwingMethod()
//   return x
// }
// CHECK-LABEL: sil hidden @$s15coverage_errors6test43SiSgyF : $@convention(thin) () -> Optional<Int>
// CHECK:       bb0:
// CHECK:       increment_profiler_counter 0
// CHECK:       [[GETTER:%[0-9]+]] = function_ref @$s15coverage_errors9throwingSAA1SVvg : $@convention(thin) () -> (S, @error any Error)
// CHECK:       try_apply [[GETTER]]() : $@convention(thin) () -> (S, @error any Error), normal [[BB_NORMAL:bb[0-9]+]], error [[BB_ERR:bb[0-9]+]]
//
// CHECK:       [[BB_ERR]]
// CHECK:       increment_profiler_counter 1
//
// CHECK:       [[BB_NORMAL]]
// CHECK:       [[METHOD:%[0-9]+]] = function_ref @$s15coverage_errors1SV14throwingMethodSiyKF : $@convention(method) (S) -> (Int, @error any Error) // user: %5
// CHECK:       try_apply [[METHOD]]({{%[0-9]+}}) : $@convention(method) (S) -> (Int, @error any Error), normal [[BB_NORMAL:bb[0-9]+]], error [[BB_ERR:bb[0-9]+]]
//
// CHECK:       [[BB_ERR]]
// CHECK:       increment_profiler_counter 2

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
func test7() -> Int32 {  // CHECK-NEXT: [[@LINE]]:23 -> [[@LINE+29]]:2 : 0
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
  }                            // CHECK-NEXT: [[@LINE]]:4 {{.*}} : (1 - 2)
  return 0
}                              // CHECK-NEXT: }

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

// TODO: We probably ought to include the function call in the non-throwing
// regions here (rdar://118524386).
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

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test24SiyKF"
func test24() throws -> Int {              // CHECK-NEXT: [[@LINE]]:29   -> [[@LINE+4]]:2  : 0
  let x = .random() ? try throwingFn() : 0 // CHECK-NEXT: [[@LINE]]:23   -> [[@LINE]]:39   : 1
  return x                                 // CHECK-NEXT: [[@LINE-1]]:39 -> [[@LINE]]:11   : (0 - 2)
                                           // CHECK-NEXT: [[@LINE-2]]:42 -> [[@LINE-2]]:43 : (0 - 1)
}                                          // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test25SiyKF"
func test25() throws -> Int {              // CHECK-NEXT: [[@LINE]]:29   -> [[@LINE+4]]:2  : 0
  let x = .random() ? 0 : try throwingFn() // CHECK-NEXT: [[@LINE]]:23   -> [[@LINE]]:24   : 1
  return x                                 // CHECK-NEXT: [[@LINE-1]]:27 -> [[@LINE-1]]:43 : (0 - 1)
                                           // CHECK-NEXT: [[@LINE-2]]:43 -> [[@LINE-1]]:11 : (0 - 2)
}                                          // CHECK-NEXT: }

// Note in this case the throws region of the first branch overlaps the
// second branch, which isn't ideal, but it matches what we already do
// for e.g if statements and returns, and doesn't impact the resulting
// coverage since we always take the counter for the smallest subrange,
// which in this case is the region for the second branch.
// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test26SiyKF"
func test26() throws -> Int {                             // CHECK-NEXT: [[@LINE]]:29   -> [[@LINE+5]]:2   : 0
  let x = .random() ? try throwingFn() : try throwingFn() // CHECK-NEXT: [[@LINE]]:23   -> [[@LINE]]:39    : 1
  return x                                                // CHECK-NEXT: [[@LINE-1]]:39 -> [[@LINE]]:11    : (0 - 2)
                                                          // CHECK-NEXT: [[@LINE-2]]:42 -> [[@LINE-2]]:58  : (0 - 1)
                                                          // CHECK-NEXT: [[@LINE-3]]:58 -> [[@LINE-2]]:11  : ((0 - 2) - 3)
}                                                         // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test27SiyKF"
func test27() throws -> Int {                         // CHECK-NEXT: [[@LINE]]:29   -> [[@LINE+5]]:2  : 0
  let x = try .random() ? throwingFn() : throwingFn() // CHECK-NEXT: [[@LINE]]:27   -> [[@LINE]]:39   : 1
  return x                                            // CHECK-NEXT: [[@LINE-1]]:39 -> [[@LINE]]:11   : (0 - 2)
                                                      // CHECK-NEXT: [[@LINE-2]]:42 -> [[@LINE-2]]:54 : (0 - 1)
                                                      // CHECK-NEXT: [[@LINE-3]]:54 -> [[@LINE-2]]:11 : ((0 - 2) - 3)
}                                                     // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test28SiyKF"
func test28() throws -> Int { // CHECK-NEXT: [[@LINE]]:29   -> [[@LINE+5]]:2  : 0
  let x = try .random()       // CHECK-NEXT: [[@LINE+1]]:7  -> [[@LINE+1]]:19 : 1
    ? throwingFn()            // CHECK-NEXT: [[@LINE]]:19   -> [[@LINE+2]]:11 : (0 - 2)
    : throwingFn()            // CHECK-NEXT: [[@LINE]]:7    -> [[@LINE]]:19   : (0 - 1)
  return x                    // CHECK-NEXT: [[@LINE-1]]:19 -> [[@LINE]]:11   : ((0 - 2) - 3)
}                             // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test29Si_SityKF"
func test29() throws -> (Int, Int) { // CHECK-NEXT: [[@LINE]]:36   -> [[@LINE+7]]:2  : 0
  let x = try .random()              // CHECK-NEXT: [[@LINE+1]]:7  -> [[@LINE+1]]:24 : 1
    ? (throwingFn(), 0)              // CHECK-NEXT: [[@LINE]]:20   -> [[@LINE]]:24   : (1 - 2)
    : (0, throwingFn())              // CHECK-NEXT: [[@LINE-1]]:24 -> [[@LINE+1]]:11 : (0 - 2)
  return x                           // CHECK-NEXT: [[@LINE-1]]:7  -> [[@LINE-1]]:24 : (0 - 1)
                                     // CHECK-NEXT: [[@LINE-2]]:23 -> [[@LINE-2]]:24 : ((0 - 1) - 3)
                                     // CHECK-NEXT: [[@LINE-3]]:24 -> [[@LINE-2]]:11 : ((0 - 2) - 3)
}                                    // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test30Si_SityKF"
func test30() throws -> (Int, Int) { // CHECK-NEXT: [[@LINE]]:36   -> [[@LINE+6]]:2  : 0
  let x = try .random()              // CHECK-NEXT: [[@LINE+1]]:7  -> [[@LINE+1]]:35 : 1
    ? (throwingFn(), throwingFn())   // CHECK-NEXT: [[@LINE]]:20   -> [[@LINE]]:35   : (1 - 2)
    : (0, 0)                         // CHECK-NEXT: [[@LINE-1]]:34 -> [[@LINE-1]]:35 : ((1 - 2) - 3)
  return x                           // CHECK-NEXT: [[@LINE-2]]:35 -> [[@LINE]]:11   : ((0 - 2) - 3)
                                     // CHECK-NEXT: [[@LINE-2]]:7  -> [[@LINE-2]]:13 : (0 - 1)
}                                    // CHECK-NEXT: }

@discardableResult
func takesOptInts(_ x: Int?, _ y: Int?) -> Int { 0 }

// `throwingFn` is within a `try?`, so the non-error branch is empty.
// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test31yyF"
func test31() {                      // CHECK-NEXT: [[@LINE]]:15 -> [[@LINE+2]]:2 : 0
  takesOptInts(try? throwingFn(), 0)
}                                    // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test32yyF"
func test32() {        // CHECK-NEXT: [[@LINE]]:15 -> [[@LINE+5]]:2 : 0
  takesOptInts(
    try? throwingFn(),
    try? throwingFn()
  )
}                      // CHECK-NEXT: }

// Here the throws region extends into the second arg.
// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test33SiSgyF"
func test33() -> Int? {              // CHECK-NEXT: [[@LINE]]:23 -> [[@LINE+2]]:2 : 0
  try? takesOptInts(throwingFn(), 0) // CHECK-NEXT: [[@LINE]]:33 -> [[@LINE]]:37  : (0 - 1)
}                                    // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test34SiSgyF"
func test34() -> Int? {                         // CHECK-NEXT: [[@LINE]]:23   -> [[@LINE+3]]:2  : 0
  try? takesOptInts(throwingFn(), throwingFn()) // CHECK-NEXT: [[@LINE]]:33   -> [[@LINE]]:48   : (0 - 1)
                                                // CHECK-NEXT: [[@LINE-1]]:47 -> [[@LINE-1]]:48 : ((0 - 1) - 2)
}                                               // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test35SiSgyF"
func test35() -> Int? { // CHECK-NEXT: [[@LINE]]:23 -> [[@LINE+5]]:2 : 0
  try? takesOptInts(
    throwingFn(),       // CHECK-NEXT: [[@LINE]]:17 -> [[@LINE+2]]:4 : (0 - 1)
    throwingFn()        // CHECK-NEXT: [[@LINE]]:17 -> [[@LINE+1]]:4 : ((0 - 1) - 2)
  )
}                       // CHECK-NEXT: }

// The 'try's here are redundant.
// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test36SiSgyF"
func test36() -> Int? { // CHECK-NEXT: [[@LINE]]:23 -> [[@LINE+5]]:2 : 0
  try? takesOptInts(
    try throwingFn(),   // CHECK-NEXT: [[@LINE]]:21 -> [[@LINE+2]]:4 : (0 - 1)
    try throwingFn()    // CHECK-NEXT: [[@LINE]]:21 -> [[@LINE+1]]:4 : ((0 - 1) - 2)
  )
}                       // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test37SiSgyF"
func test37() -> Int? { // CHECK-NEXT: [[@LINE]]:23 -> [[@LINE+5]]:2 : 0
  try? takesOptInts(
    try throwingFn(),   // CHECK-NEXT: [[@LINE]]:21 -> [[@LINE+2]]:4 : (0 - 1)
    try? S[]
  )
}                       // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test38SiSgyF"
func test38() -> Int? { // CHECK-NEXT: [[@LINE]]:23 -> [[@LINE+5]]:2 : 0
  try? takesOptInts(
    try? throwingFn(),
    try S[]             // CHECK-NEXT: [[@LINE]]:12 -> [[@LINE+1]]:4 : (0 - 2)
  )
}                       // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test39Si_SitSgyF"
func test39(
) -> (Int, Int)? {    // CHECK-NEXT: [[@LINE]]:18 -> [[@LINE+5]]:2 : 0
  try? (takesOptInts(
    throwingFn(),     // CHECK-NEXT: [[@LINE]]:17 -> [[@LINE+2]]:8 : (0 - 1)
    try? throwingProp
  ), 0)
}                     // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test40SiyKF"
func test40(
) throws -> Int {      // CHECK-NEXT: [[@LINE]]:17 -> [[@LINE+5]]:2 : 0
  try takesOptInts(
    try? throwingFn(),
    throwingProp       // CHECK-NEXT: [[@LINE]]:17 -> [[@LINE+2]]:2 : (0 - 2)
  )
}                      // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test41SiyKF"
func test41(
) throws -> Int {      // CHECK-NEXT: [[@LINE]]:17 -> [[@LINE+5]]:2 : 0
  try takesOptInts(
    throwingFn(),      // CHECK-NEXT: [[@LINE]]:17 -> [[@LINE+3]]:2 : (0 - 1)
    try? throwingFn()
  )
}                      // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test42SiSgyF"
func test42() -> Int? {                  // CHECK-NEXT: [[@LINE]]:23 -> [[@LINE+5]]:2  : 0
  guard let x = try? throwingFn() else { // CHECK-NEXT: [[@LINE]]:40 -> [[@LINE+2]]:4  : 1
    return nil
  }                                      // CHECK-NEXT: [[@LINE]]:4  -> [[@LINE+1]]:11 : (0 - 1)
  return x
}                                        // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test43SiSgyF"
func test43() -> Int? {                   // CHECK-NEXT: [[@LINE]]:23 -> [[@LINE+3]]:2 : 0
  let x = try? throwingS.throwingMethod() // CHECK-NEXT: [[@LINE]]:25 -> [[@LINE]]:42  : (0 - 1)
  return x
}                                         // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test44SiSgyKF"
func test44() throws -> Int? {                   // CHECK-NEXT: [[@LINE]]:30 -> [[@LINE+3]]:2  : 0
  let x = try (try? throwingS)?.throwingMethod() // CHECK-NEXT: [[@LINE]]:49 -> [[@LINE+1]]:11 : (0 - 2)
  return x
}

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test45yyF"
func test45() {                      // CHECK-NEXT: [[@LINE]]:15 -> [[@LINE+2]]:2 : 0
  _ = try? { throw SomeErr.Err1 }()
}                                    // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test45yyFyyKXEfU_"
// CHECK-NEXT:    [[@LINE-4]]:12 -> [[@LINE-4]]:34 : 0
// CHECK-NEXT:  }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test46Si_SitSgyF"
func test46() -> (Int, Int)? {       // CHECK-NEXT: [[@LINE]]:30 -> [[@LINE+2]]:2 : 0
  try? ({ throw SomeErr.Err1 }(), 0) // CHECK-NEXT: [[@LINE]]:33 -> [[@LINE]]:37  : (0 - 1)
}                                    // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test47yyKF"
func test47() throws { // CHECK-NEXT: [[@LINE]]:22 -> [[@LINE+2]]:2 : 0
  try throwingFn()     // CHECK-NEXT: [[@LINE]]:19 -> [[@LINE+1]]:2 : (0 - 1)
}                      // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test48SiyKF"
func test48() throws -> Int { // CHECK-NEXT: [[@LINE]]:29   -> [[@LINE+5]]:2 : 0
  try throwingBool()          // CHECK-NEXT: [[@LINE]]:21   -> [[@LINE+4]]:2 : (0 - 2)
    ? try throwingFn()        // CHECK-NEXT: [[@LINE]]:7    -> [[@LINE]]:23  : 1
    : 1                       // CHECK-NEXT: [[@LINE-1]]:23 -> [[@LINE+2]]:2 : ((0 - 2) - 3)
                              // CHECK-NEXT: [[@LINE-1]]:7  -> [[@LINE-1]]:8 : ((0 - 1) - 2)
}                             // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test49SiyKF"
func test49() throws -> Int { // CHECK-NEXT: [[@LINE]]:29   -> [[@LINE+6]]:2  : 0
  try throwingBool()          // CHECK-NEXT: [[@LINE]]:21   -> [[@LINE+5]]:2  : (0 - 2)
    ? try throwingFn()        // CHECK-NEXT: [[@LINE]]:7    -> [[@LINE]]:23   : 1
    : try throwingFn()        // CHECK-NEXT: [[@LINE-1]]:23 -> [[@LINE+3]]:2  : ((0 - 2) - 3)
                              // CHECK-NEXT: [[@LINE-1]]:7  -> [[@LINE-1]]:23 : ((0 - 1) - 2)
                              // CHECK-NEXT: [[@LINE-2]]:23 -> [[@LINE+1]]:2  : (((0 - 2) - 3) - 4)
}                             // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test50SiyKF"
func test50() throws -> Int {     // CHECK-NEXT: [[@LINE]]:29   -> [[@LINE+7]]:2  : 0
  let x = if try throwingBool() { // CHECK-NEXT: [[@LINE]]:14   -> [[@LINE]]:32   : 0
    try throwingFn()              // CHECK-NEXT: [[@LINE-1]]:32 -> [[@LINE+4]]:11 : (0 - 2)
  } else {                        // CHECK-NEXT: [[@LINE-2]]:33 -> [[@LINE]]:4    : 1
    1                             // CHECK-NEXT: [[@LINE-2]]:21 -> [[@LINE-1]]:4  : (1 - 3)
  }                               // CHECK-NEXT: [[@LINE-2]]:10 -> [[@LINE]]:4    : ((0 - 1) - 2)
  return x                        // CHECK-NEXT: [[@LINE-1]]:4  -> [[@LINE]]:11   : ((0 - 2) - 3)
}                                 // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test51SiyKF"
func test51() throws -> Int {     // CHECK-NEXT: [[@LINE]]:29   -> [[@LINE+8]]:2  : 0
  let x = if try throwingBool() { // CHECK-NEXT: [[@LINE]]:14   -> [[@LINE]]:32   : 0
    try throwingFn()              // CHECK-NEXT: [[@LINE-1]]:32 -> [[@LINE+4]]:11 : (0 - 2)
  } else {                        // CHECK-NEXT: [[@LINE-2]]:33 -> [[@LINE]]:4    : 1
    try throwingFn()              // CHECK-NEXT: [[@LINE-2]]:21 -> [[@LINE-1]]:4  : (1 - 3)
  }                               // CHECK-NEXT: [[@LINE-2]]:10 -> [[@LINE]]:4    : ((0 - 1) - 2)
  return x                        // CHECK-NEXT: [[@LINE-2]]:21 -> [[@LINE-1]]:4  : (((0 - 1) - 2) - 4)
                                  // CHECK-NEXT: [[@LINE-2]]:4  -> [[@LINE-1]]:11 : (((0 - 2) - 3) - 4)
}                                 // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test52SiyKF"
func test52() throws -> Int {     // CHECK-NEXT: [[@LINE]]:29   -> [[@LINE+9]]:2  : 0
  let x = if try throwingBool(),  // CHECK-NEXT: [[@LINE]]:14   -> [[@LINE]]:32   : 0
             try throwingBool() { // CHECK-NEXT: [[@LINE-1]]:32 -> [[@LINE+5]]:11 : (0 - 2)
    try throwingFn()              // CHECK-NEXT: [[@LINE-1]]:32 -> [[@LINE+4]]:11 : ((0 - 2) - 3)
  } else {                        // CHECK-NEXT: [[@LINE-2]]:33 -> [[@LINE]]:4    : 1
    try throwingFn()              // CHECK-NEXT: [[@LINE-2]]:21 -> [[@LINE-1]]:4  : (1 - 4)
  }                               // CHECK-NEXT: [[@LINE-2]]:10 -> [[@LINE]]:4    : (((0 - 1) - 2) - 3)
  return x                        // CHECK-NEXT: [[@LINE-2]]:21 -> [[@LINE-1]]:4  : ((((0 - 1) - 2) - 3) - 5)
                                  // CHECK-NEXT: [[@LINE-2]]:4  -> [[@LINE-1]]:11 : ((((0 - 2) - 3) - 4) - 5)
}                                 // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test53yyKF"
func test53() throws {     // CHECK-NEXT: [[@LINE]]:22   -> [[@LINE+9]]:2 : 0
  if try throwingBool(),   // CHECK-NEXT: [[@LINE]]:6    -> [[@LINE]]:24  : 0
      try throwingBool() { // CHECK-NEXT: [[@LINE-1]]:24 -> [[@LINE+7]]:2 : (0 - 2)
    try throwingFn()       // CHECK-NEXT: [[@LINE-1]]:25 -> [[@LINE+6]]:2 : ((0 - 2) - 3)
  } else {                 // CHECK-NEXT: [[@LINE-2]]:26 -> [[@LINE]]:4   : 1
    try throwingFn()       // CHECK-NEXT: [[@LINE-2]]:21 -> [[@LINE-1]]:4 : (1 - 4)
  }                        // CHECK-NEXT: [[@LINE-2]]:10 -> [[@LINE]]:4   : (((0 - 1) - 2) - 3)
                           // CHECK-NEXT: [[@LINE-2]]:21 -> [[@LINE-1]]:4 : ((((0 - 1) - 2) - 3) - 5)
                           // CHECK-NEXT: [[@LINE-2]]:4  -> [[@LINE+1]]:2 : ((((0 - 2) - 3) - 4) - 5)
}                          // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test54SiSgyF"
func test54()-> Int? {            // CHECK-NEXT: [[@LINE]]:22 -> [[@LINE+7]]:2 : 0
  if let x = try? throwingFn(),
      let y = try? throwingFn() { // CHECK-NEXT: [[@LINE]]:33 -> [[@LINE+2]]:4 : 1
    x + y
  } else {                        // CHECK-NEXT: [[@LINE]]:10 -> [[@LINE+2]]:4 : (0 - 1)
    try? throwingFn()
  }
}                                 // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test55yyKF"
func test55() throws {       // CHECK-NEXT: [[@LINE]]:22   -> [[@LINE+5]]:2 : 0
  while try throwingBool() { // CHECK-NEXT: [[@LINE]]:9    -> [[@LINE]]:27  : (0 + 1)
                             // CHECK-NEXT: [[@LINE-1]]:27 -> [[@LINE+3]]:2 : (0 - 2)
                             // CHECK-NEXT: [[@LINE-2]]:28 -> [[@LINE+1]]:4 : 1
  }                          // CHECK-NEXT: [[@LINE]]:4    -> [[@LINE+1]]:2 : (0 - 2)
}                            // CHECK-NEXT: }

// FIXME: The second condition ought to get a region too (rdar://118649481)
// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test56yyKF"
func test56() throws {       // CHECK-NEXT: [[@LINE]]:22   -> [[@LINE+6]]:2 : 0
  while try throwingBool(),  // CHECK-NEXT: [[@LINE]]:9    -> [[@LINE]]:27  : (0 + 1)
        try throwingBool() { // CHECK-NEXT: [[@LINE-1]]:27 -> [[@LINE+4]]:2 : (0 - 2)
                             // CHECK-NEXT: [[@LINE-1]]:27 -> [[@LINE+3]]:2 : ((0 - 2) - 3)
                             // CHECK-NEXT: [[@LINE-2]]:28 -> [[@LINE+1]]:4 : 1
  }                          // CHECK-NEXT: [[@LINE]]:4    -> [[@LINE+1]]:2 : ((0 - 2) - 3)
}                            // CHECK-NEXT: }

// FIXME: We ought to be giving both conditions a region here...
// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test57yyF"
func test57() {                     // CHECK-NEXT: [[@LINE]]:15 -> [[@LINE+4]]:2 : 0
  while let _ = try? throwingFn(),
        let _ = try? throwingFn() { // CHECK-NEXT: [[@LINE]]:35 -> [[@LINE+1]]:4 : 1
  }                                 // CHECK-NEXT: [[@LINE]]:4  -> [[@LINE+1]]:2 : 0
}                                   // CHECK-NEXT: }

// We generate the same regions for `try!` as `try`. We could handle it
// specially, but the error branches are effectively unreachable, so it
// doesn't make a difference.
// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test58SiyF"
func test58() -> Int {      // CHECK-NEXT: [[@LINE]]:22 -> [[@LINE+3]]:2  : 0
  let x = try! throwingFn() // CHECK-NEXT: [[@LINE]]:28 -> [[@LINE+1]]:11 : (0 - 1)
  return x
}                           // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test59SiyKF"
func test59() throws -> Int {     // CHECK-NEXT: [[@LINE]]:29   -> [[@LINE+5]]:2  : 0
  guard try throwingBool() else { // CHECK-NEXT: [[@LINE]]:27   -> [[@LINE+3]]:11 : (0 - 2)
    return 1                      // CHECK-NEXT: [[@LINE-1]]:33 -> [[@LINE+1]]:4  : 1
  }                               // CHECK-NEXT: [[@LINE]]:4    -> [[@LINE+1]]:11 : ((0 - 1) - 2)
  return 0
}                                 // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test60SiyKF"
func test60() throws -> Int { // CHECK-NEXT: [[@LINE]]:29   -> [[@LINE+7]]:2  : 0
  switch try throwingBool() { // CHECK-NEXT: [[@LINE]]:10   -> [[@LINE]]:28   : 0
  case true:                  // CHECK-NEXT: [[@LINE-1]]:28 -> [[@LINE+5]]:2  : (0 - 1)
    return 0                  // CHECK-NEXT: [[@LINE-1]]:3  -> [[@LINE]]:13   : 2
  case false:                 // CHECK-NEXT: [[@LINE]]:3    -> [[@LINE+1]]:13 : 3
    return 1
  }
}                             // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test61SiyKF"
func test61() throws -> Int { // CHECK-NEXT: [[@LINE]]:29   -> [[@LINE+7]]:2 : 0
  switch try throwingBool() { // CHECK-NEXT: [[@LINE]]:10   -> [[@LINE]]:28  : 0
  case true:                  // CHECK-NEXT: [[@LINE-1]]:28 -> [[@LINE+5]]:2 : (0 - 1)
    0                         // CHECK-NEXT: [[@LINE-1]]:3  -> [[@LINE]]:6   : 2
  case false:                 // CHECK-NEXT: [[@LINE]]:3    -> [[@LINE+1]]:6 : 3
    1                         // FIXME: This next region shouldn't be needed, we should know it's unrechabale (rdar://118653218).
  }                           // CHECK-NEXT: [[@LINE]]:4    -> [[@LINE+1]]:2 : (2 + 3)
}                             // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test62SiyKF"
func test62() throws -> Int {         // CHECK-NEXT: [[@LINE]]:29   -> [[@LINE+8]]:2  : 0
  let x = switch try throwingBool() { // CHECK-NEXT: [[@LINE]]:18   -> [[@LINE]]:36   : 0
  case true:                          // CHECK-NEXT: [[@LINE-1]]:36 -> [[@LINE+5]]:11 : (0 - 1)
    0                                 // CHECK-NEXT: [[@LINE-1]]:3  -> [[@LINE]]:6    : 2
  case false:                         // CHECK-NEXT: [[@LINE]]:3    -> [[@LINE+1]]:6  : 3
    1
  }                                   // CHECK-NEXT: [[@LINE]]:4    -> [[@LINE+1]]:11 : (2 + 3)
  return x
}                                     // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test63yyKF"
func test63() throws {          // CHECK-NEXT: [[@LINE]]:22   -> [[@LINE+4]]:2 : 0
  for _ in [try throwingFn()] { // CHECK-NEXT: [[@LINE]]:29   -> [[@LINE+3]]:2 : (0 - 2)
                                // CHECK-NEXT: [[@LINE-1]]:31 -> [[@LINE+1]]:4 : 1
  }                             // CHECK-NEXT: [[@LINE]]:4    -> [[@LINE+1]]:2 : (0 - 2)
}                               // CHECK-NEXT: }

// FIXME: We don't currently assign a separate region for the where clause, but
// we ought to (rdar://118653191).
// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test64yyKF"
func test64() throws {       // CHECK-NEXT: [[@LINE]]:22   -> [[@LINE+5]]:2 : 0
  for _ in [0]
  where try throwingBool() { // CHECK-NEXT: [[@LINE]]:27   -> [[@LINE+3]]:2 : (0 - 2)
                             // CHECK-NEXT: [[@LINE-1]]:28 -> [[@LINE+1]]:4 : 1
  }                          // CHECK-NEXT: [[@LINE]]:4    -> [[@LINE+1]]:2 : (0 - 2)
}                            // CHECK-NEXT: }

// CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors6test65yyKF"
func test65() throws {       // CHECK-NEXT: [[@LINE]]:22   -> [[@LINE+5]]:2 : 0
  repeat {                   // CHECK-NEXT: [[@LINE]]:10   -> [[@LINE+2]]:4 : 1
                             // CHECK-NEXT: [[@LINE+1]]:4  -> [[@LINE+3]]:2 : 0
  } while try throwingBool() // CHECK-NEXT: [[@LINE]]:11   -> [[@LINE]]:29  : 1
                             // CHECK-NEXT: [[@LINE-1]]:29 -> [[@LINE+1]]:2 : (0 - 2)
}                            // CHECK-NEXT: }

struct TestType1 {
  // CHECK-LABEL: sil_coverage_map {{.*}}// coverage_errors.TestType1.init() -> coverage_errors.TestType1
  init() {               // CHECK-NEXT: [[@LINE]]:10 -> [[@LINE+5]]:4 : 0
    do {                 // CHECK-NEXT: [[@LINE]]:8  -> [[@LINE+2]]:6 : 0
      throw SomeErr.Err1
    } catch {            // CHECK-NEXT: [[@LINE]]:13 -> [[@LINE+1]]:6 : 1
    }                    // CHECK-NEXT: [[@LINE]]:6  -> [[@LINE+1]]:4 : 1
  }                      // CHECK-NEXT: }
}

@propertyWrapper
struct Wrapper<T> {
  var wrappedValue: T

  init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }

  init(wrappedValue: T, x: T, y: T? = nil) {
    self.wrappedValue = wrappedValue
  }
}

struct TestType2 {
  let a = try? throwingFn()
  // CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors9TestType2V1aSiSgvpfi"
  // CHECK-NEXT: [[@LINE-2]]:11 -> [[@LINE-2]]:28 : 0
  // CHECK-NEXT: }

  let b = try? (throwingFn(), 0)
  // CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors9TestType2V1bSi_SitSgvpfi"
  // CHECK-NEXT:   [[@LINE-2]]:11 -> [[@LINE-2]]:33 : 0
  // CHECK-NEXT:   [[@LINE-3]]:29 -> [[@LINE-3]]:33 : (0 - 1)
  // CHECK-NEXT: }

  let c = try? (throwingFn(), .random() ? 0 : 1)
  // CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors9TestType2V1cSi_SitSgvpfi"
  // CHECK-NEXT:  [[@LINE-2]]:11 -> [[@LINE-2]]:49 : 0
  // CHECK-NEXT:  [[@LINE-3]]:29 -> [[@LINE-3]]:49 : (0 - 1)
  // CHECK-NEXT:  [[@LINE-4]]:43 -> [[@LINE-4]]:44 : 2
  // CHECK-NEXT:  [[@LINE-5]]:47 -> [[@LINE-5]]:48 : ((0 - 1) - 2)
  // CHECK-NEXT: }

  let d = (try? (throwingFn(), .random() ? 0 : 1), 0)
  // CHECK-LABEL: sil_coverage_map {{.*}} "$s15coverage_errors9TestType2V1dSi_SitSg_Sitvpfi"
  // CHECK-NEXT:  [[@LINE-2]]:11 -> [[@LINE-2]]:54 : 0
  // CHECK-NEXT:  [[@LINE-3]]:30 -> [[@LINE-3]]:50 : (0 - 1)
  // CHECK-NEXT:  [[@LINE-4]]:44 -> [[@LINE-4]]:45 : 2
  // CHECK-NEXT:  [[@LINE-5]]:48 -> [[@LINE-5]]:49 : ((0 - 1) - 2)
  // CHECK-NEXT: }
}

// rdar://118939162 - Make sure we don't crash when generating coverage for 'try!' in a property wrapper init.
struct TestType3 {
  // CHECK-LABEL: sil_coverage_map {{.*}} // property wrapper backing initializer of coverage_errors.TestType3.x
  // CHECK-NEXT:  [[@LINE+2]]:4 -> [[@LINE+2]]:13 : 0
  // CHECK-NEXT:  }
  @Wrapper()
  var x = try! throwingFn()
  // CHECK-LABEL: sil_coverage_map {{.*}} // variable initialization expression of coverage_errors.TestType3.(_x
  // CHECK-NEXT:  [[@LINE-2]]:11 -> [[@LINE-2]]:28 : 0
  // CHECK-NEXT:  }
}

struct TestType4 {
  // CHECK-LABEL: sil_coverage_map {{.*}} // property wrapper backing initializer of coverage_errors.TestType4.x
  // CHECK-NEXT:  [[@LINE+2]]:4  -> [[@LINE+2]]:13 : 0
  // CHECK-NEXT:  }
  @Wrapper()
  var x = try! (throwingFn(), 0)
  // CHECK-LABEL: sil_coverage_map {{.*}} // variable initialization expression of coverage_errors.TestType4.(_x
  // CHECK-NEXT:  [[@LINE-2]]:11 -> [[@LINE-2]]:33 : 0
  // CHECK-NEXT:  [[@LINE-3]]:29 -> [[@LINE-3]]:33 : (0 - 1)
  // CHECK-NEXT:  }
}

struct TestType5 {
  // CHECK-LABEL: sil_coverage_map {{.*}} // property wrapper backing initializer of coverage_errors.TestType5.x
  // CHECK-NEXT:  [[@LINE+3]]:4  -> [[@LINE+3]]:33 : 0
  // CHECK-NEXT:  [[@LINE+2]]:32 -> [[@LINE+2]]:33 : (0 - 1)
  // CHECK-NEXT:  }
  @Wrapper(x: try! throwingFn())
  var x = 0
  // CHECK-LABEL: sil_coverage_map {{.*}} // variable initialization expression of coverage_errors.TestType5.(_x
  // CHECK-NEXT:  [[@LINE-2]]:11 -> [[@LINE-2]]:12 : 0
  // CHECK-NEXT:  }
}

struct TestType6 {
  // CHECK-LABEL: sil_coverage_map {{.*}} // property wrapper backing initializer of coverage_errors.TestType6.x
  // CHECK-NEXT:  [[@LINE+3]]:4  -> [[@LINE+3]]:39 : 0
  // CHECK-NEXT:  [[@LINE+2]]:32 -> [[@LINE+2]]:39 : (0 - 1)
  // CHECK-NEXT:  }
  @Wrapper(x: try! throwingFn(), y: 0)
  var x = 0
  // CHECK-LABEL: sil_coverage_map {{.*}} // variable initialization expression of coverage_errors.TestType6.(_x
  // CHECK-NEXT:  [[@LINE-2]]:11 -> [[@LINE-2]]:12 : 0
  // CHECK-NEXT:  }
}

struct TestType7 {
  // CHECK-LABEL: sil_coverage_map {{.*}} // property wrapper backing initializer of coverage_errors.TestType7.x
  // CHECK-NEXT:  [[@LINE+3]]:4  -> [[@LINE+3]]:49 : 0
  // CHECK-NEXT:  [[@LINE+2]]:33 -> [[@LINE+2]]:49 : (0 - 1)
  // CHECK-NEXT:  }
  @Wrapper(x: try! (throwingFn(), 0), y: (0, 0))
  var x = try! (throwingFn(), 0)
  // CHECK-LABEL: sil_coverage_map {{.*}} // variable initialization expression of coverage_errors.TestType7.(_x
  // CHECK-NEXT:  [[@LINE-2]]:11 -> [[@LINE-2]]:33 : 0
  // CHECK-NEXT:  [[@LINE-3]]:29 -> [[@LINE-3]]:33 : (0 - 1)
  // CHECK-NEXT:  }
}
