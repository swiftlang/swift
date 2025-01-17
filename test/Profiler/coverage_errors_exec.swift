// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -profile-generate -profile-coverage-mapping -o %t/main

// RUN: %target-codesign %t/main
// RUN: env %env-LLVM_PROFILE_FILE=%t/default.profraw %target-run %t/main

// RUN: %llvm-profdata merge %t/default.profraw -o %t/default.profdata
// RUN: %llvm-cov show %t/main -instr-profile=%t/default.profdata | %FileCheck %s

// REQUIRES: profile_runtime
// REQUIRES: executable_test

struct Err: Error {}

struct S {
  static subscript() -> Int {
    get throws { throw Err() }
  }
  func throwingMethod() throws -> Int { throw Err() }
}

func noThrowingFn() throws -> Int { 0 }

@discardableResult
func throwingFn() throws -> Int { throw Err() }

func throwingBool() throws -> Bool { throw Err() }
func noThrowingBool() throws -> Bool { true }

var throwingProp: Int {
  get throws { throw Err() }
}

var throwingS: S {
  get throws { throw Err() }
}

var noThrowingS: S {
  get throws { S() }
}

func test1() -> Int {        // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  do {                       // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    let x = try throwingFn() // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    return x                 // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  } catch {                  // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    return 0                 // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  }                          // CHECK: {{ *}}[[@LINE]]|{{ *}}1
}                            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = test1()                  // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test2() throws -> Int { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  let x = try throwingFn()   // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return x                   // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test2()             // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test3() throws -> Int { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  let x = try throwingProp   // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return x                   // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test3()             // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test4() throws -> Int { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  let x = try S[]            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return x                   // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test4()             // CHECK: {{ *}}[[@LINE]]|{{ *}}1

// Note we don't emit a region after the call since it would be empty.
func test5() -> Int {        // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  do {                       // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    return try throwingFn()  // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  } catch {                  // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    return 0                 // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  }                          // CHECK: {{ *}}[[@LINE]]|{{ *}}1
}                            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = test5()                  // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func takesInts(_ x: Int, _ y: Int) {} // CHECK: {{ *}}[[@LINE]]|{{ *}}0

// The throwing expr is nested here, the region starts after the throwing expr.
func test6() throws {            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  takesInts(try throwingFn(), 0) // CHECK: {{ *}}[[@LINE]]|{{ *}}1
}                                // CHECK: {{ *}}[[@LINE]]|{{ *}}0
try? test6()                     // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test7() throws {            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  takesInts(                     // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try throwingFn(),            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    0                            // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  )                              // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                                // CHECK: {{ *}}[[@LINE]]|{{ *}}0
try? test7()                     // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test8() throws -> Int {             // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  let x = try throwingS.throwingMethod() // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return x                               // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                                        // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test8()

func test9() throws -> Int {               // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  let x = try noThrowingS.throwingMethod() // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return x                                 // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                                          // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test9()                           // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test10() throws { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  takesInts(           // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try throwingFn(),  // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try throwingFn()   // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  )                    // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                      // CHECK: {{ *}}[[@LINE]]|{{ *}}0
try? test10()          // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test11() throws {  // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  takesInts(            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try noThrowingFn(), // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try throwingFn()    // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  )                     // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                       // CHECK: {{ *}}[[@LINE]]|{{ *}}0
try? test11()           // CHECK: {{ *}}[[@LINE]]|{{ *}}1

// rdar://100470244 - Make sure we don't underflow the counter here.
func test12() -> Int { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  do {                 // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try throwingFn()   // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    return 1           // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  } catch {            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    return 2           // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  }                    // CHECK: {{ *}}[[@LINE]]|{{ *}}1
}                      // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = test12()           // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test13() throws -> Int { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  x: do {                     // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try throwingFn()          // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  } catch is Err {            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    break x                   // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  }                           // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return 1                    // CHECK: {{ *}}[[@LINE]]|{{ *}}1
}                             // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test13()             // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test14() throws -> Int { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  x: do {                     // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    do {                      // CHECK: {{ *}}[[@LINE]]|{{ *}}1
      try throwingFn()        // CHECK: {{ *}}[[@LINE]]|{{ *}}1
      break x                 // CHECK: {{ *}}[[@LINE]]|{{ *}}0
    } catch is Err {          // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    }                         // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    return 1                  // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  } catch is Err {            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  }                           // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  return 2                    // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                             // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test14()             // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test15() throws -> Int { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  x: do {                     // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    do {                      // CHECK: {{ *}}[[@LINE]]|{{ *}}1
      try noThrowingFn()      // CHECK: {{ *}}[[@LINE]]|{{ *}}1
      break x                 // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    } catch is Err {          // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    }                         // CHECK: {{ *}}[[@LINE]]|{{ *}}0
    return 1                  // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  } catch is Err {            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
                              // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  }                           // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return 2                    // CHECK: {{ *}}[[@LINE]]|{{ *}}1
}                             // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test15()             // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test16() throws -> Int { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  let x = try true            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    ? throwingFn()            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    : throwingFn()            // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  return x                    // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                             // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test16()             // CHECK: {{ *}}[[@LINE]]|{{ *}}1

// FIXME: The line execution count is misleading here as it includes the
// trailing edge of the thrown error from the first branch (rdar://118654503).
func test17() throws -> Int { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  let x = try false           // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    ? throwingFn()            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    : throwingFn()            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return x                    // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                             // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test17()             // CHECK: {{ *}}[[@LINE]]|{{ *}}1

// FIXME: The line execution count is misleading here as it includes the
// trailing edge of the thrown error from the first branch (rdar://118654503).
func test18() throws -> Int { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  let x = try true            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    ? noThrowingFn()          // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    : noThrowingFn()          // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return x                    // CHECK: {{ *}}[[@LINE]]|{{ *}}1
}                             // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test18()             // CHECK: {{ *}}[[@LINE]]|{{ *}}1

@discardableResult
func takesOptInts(_ x: Int?, _ y: Int?) -> Int { 0 }

func test19() {        // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  takesOptInts(        // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try? throwingFn(), // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try? throwingFn()  // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  )                    // CHECK: {{ *}}[[@LINE]]|{{ *}}1
}                      // CHECK: {{ *}}[[@LINE]]|{{ *}}1
test19()               // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test20(
) throws -> Int {     // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  try takesOptInts(   // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    throwingFn(),     // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try? throwingFn() // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  )                   // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                     // CHECK: {{ *}}[[@LINE]]|{{ *}}0
_ = try? test20()     // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test21() throws -> Int { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  try {                       // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    throw Err()               // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  }()                         // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return 1                    // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                             // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test21()             // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test22(
) throws -> Int {     // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  try takesOptInts(   // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    noThrowingFn(),   // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try? throwingFn() // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  )                   // CHECK: {{ *}}[[@LINE]]|{{ *}}1
}                     // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test22()     // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test23() -> Int? {                  // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  guard let x = try? throwingFn() else { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    return nil                           // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  }                                      // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return x                               // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                                        // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = test23()                             // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test24() -> Int? {                    // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  guard let x = try? noThrowingFn() else { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    return nil                             // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  }                                        // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return x                                 // CHECK: {{ *}}[[@LINE]]|{{ *}}1
}                                          // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = test24()                               // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test25() -> Int {        // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  let x = try! noThrowingFn() // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return x                    // CHECK: {{ *}}[[@LINE]]|{{ *}}1
}                             // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = test25()                  // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test26() throws -> Int {
  let x = if try throwingBool() { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try throwingFn()              // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  } else {                        // CHECK: {{ *}}[[@LINE]]|{{ *}}0
    try throwingFn()              // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  }                               // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  return x                        // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                                 // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test26()                 // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test27() throws -> Int {
  let x = if try noThrowingBool() { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try throwingFn()                // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  } else {                          // CHECK: {{ *}}[[@LINE]]|{{ *}}0
    try throwingFn()                // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  }                                 // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  return x                          // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                                   // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test27()                   // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test28() throws -> Int {
  let x = if try !noThrowingBool() { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try throwingFn()                 // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  } else {                           // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try throwingFn()                 // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  }                                  // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  return x                           // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                                    // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test28()                    // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test29() throws -> Int { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  try throwingBool()          // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    ? try throwingFn()        // CHECK: {{ *}}[[@LINE]]|{{ *}}0
    : try throwingFn()        // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                             // CHECK: {{ *}}[[@LINE]]|{{ *}}0
_ = try? test29()             // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test30() throws -> Int { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  try noThrowingBool()        // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    ? try throwingFn()        // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    : try throwingFn()        // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                             // CHECK: {{ *}}[[@LINE]]|{{ *}}0
_ = try? test30()             // CHECK: {{ *}}[[@LINE]]|{{ *}}1

// FIXME: The line execution count is misleading here (rdar://118654503).
func test31() throws -> Int { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  try !noThrowingBool()       // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    ? try throwingFn()        // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    : try throwingFn()        // CHECK: {{ *}}[[@LINE]]|{{ *}}1
}                             // CHECK: {{ *}}[[@LINE]]|{{ *}}0
_ = try? test31()             // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test32() throws {     // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  if try throwingBool(),   // CHECK: {{ *}}[[@LINE]]|{{ *}}1
      try throwingBool() { // CHECK: {{ *}}[[@LINE]]|{{ *}}0
    try throwingFn()       // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  } else {                 // CHECK: {{ *}}[[@LINE]]|{{ *}}0
    try throwingFn()       // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  }                        // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                          // CHECK: {{ *}}[[@LINE]]|{{ *}}0
_ = try? test32()          // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test33() throws {     // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  if try noThrowingBool(), // CHECK: {{ *}}[[@LINE]]|{{ *}}1
      try throwingBool() { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try throwingFn()       // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  } else {                 // CHECK: {{ *}}[[@LINE]]|{{ *}}0
    try throwingFn()       // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  }                        // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                          // CHECK: {{ *}}[[@LINE]]|{{ *}}0
_ = try? test33()          // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test34() throws {       // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  if try noThrowingBool(),   // CHECK: {{ *}}[[@LINE]]|{{ *}}1
      try noThrowingBool() { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try throwingFn()         // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  } else {                   // CHECK: {{ *}}[[@LINE]]|{{ *}}0
    try throwingFn()         // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  }                          // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                            // CHECK: {{ *}}[[@LINE]]|{{ *}}0
_ = try? test34()            // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test35() throws {       // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  if try !noThrowingBool(),  // CHECK: {{ *}}[[@LINE]]|{{ *}}1
      try noThrowingBool() { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try throwingFn()         // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  } else {                   // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    try throwingFn()         // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  }                          // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                            // CHECK: {{ *}}[[@LINE]]|{{ *}}0
_ = try? test35()            // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test36() throws -> Int {     // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  guard try throwingBool() else { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    return 1                      // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  }                               // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  return 0                        // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                                 // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test36()                 // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test37() throws -> Int {       // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  guard try noThrowingBool() else { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    return 1                        // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  }                                 // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return 0                          // CHECK: {{ *}}[[@LINE]]|{{ *}}1
}                                   // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test37()                   // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test38() throws -> Int {        // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  guard try !noThrowingBool() else { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    return 1                         // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  }                                  // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return 0                           // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                                    // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test38()                    // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test39() throws -> Int { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  switch try throwingBool() { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  case true:                  // CHECK: {{ *}}[[@LINE]]|{{ *}}0
    return 0                  // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  case false:                 // CHECK: {{ *}}[[@LINE]]|{{ *}}0
    return 1                  // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  }                           // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                             // CHECK: {{ *}}[[@LINE]]|{{ *}}0
_ = try? test39()             // CHECK: {{ *}}[[@LINE]]|{{ *}}1

// FIXME: The line coverage for the second case isn't great (rdar://118654503).
func test40() throws -> Int {   // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  switch try noThrowingBool() { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  case true:                    // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    return 0                    // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  case false:                   // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    return 1                    // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  }                             // CHECK: {{ *}}[[@LINE]]|{{ *}}1
}                               // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test40()               // CHECK: {{ *}}[[@LINE]]|{{ *}}1

// FIXME: The line coverage for the first case isn't great (rdar://118654503).
func test41() throws -> Int {    // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  switch try !noThrowingBool() { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  case true:                     // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    return 0                     // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  case false:                    // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    return 1                     // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  }                              // CHECK: {{ *}}[[@LINE]]|{{ *}}1
}                                // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test41()                // CHECK: {{ *}}[[@LINE]]|{{ *}}1

func test42() throws -> Int {         // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  let x = switch try throwingBool() { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  case true:                          // CHECK: {{ *}}[[@LINE]]|{{ *}}0
    0                                 // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  case false:                         // CHECK: {{ *}}[[@LINE]]|{{ *}}0
    1                                 // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  }                                   // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  return x                            // CHECK: {{ *}}[[@LINE]]|{{ *}}0
}                                     // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test42()                     // CHECK: {{ *}}[[@LINE]]|{{ *}}1

// FIXME: The line coverage for the second case isn't great (rdar://118654503).
func test43() throws -> Int {           // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  let x = switch try noThrowingBool() { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  case true:                            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    0                                   // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  case false:                           // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    1                                   // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  }                                     // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return x                              // CHECK: {{ *}}[[@LINE]]|{{ *}}1
}                                       // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test43()                       // CHECK: {{ *}}[[@LINE]]|{{ *}}1

// FIXME: The line coverage for the first case isn't great (rdar://118654503).
func test44() throws -> Int {            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  let x = switch try !noThrowingBool() { // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  case true:                             // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    0                                    // CHECK: {{ *}}[[@LINE]]|{{ *}}0
  case false:                            // CHECK: {{ *}}[[@LINE]]|{{ *}}1
    1                                    // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  }                                      // CHECK: {{ *}}[[@LINE]]|{{ *}}1
  return x                               // CHECK: {{ *}}[[@LINE]]|{{ *}}1
}                                        // CHECK: {{ *}}[[@LINE]]|{{ *}}1
_ = try? test44()                        // CHECK: {{ *}}[[@LINE]]|{{ *}}1
