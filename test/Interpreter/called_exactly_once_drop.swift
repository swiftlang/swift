// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-future-triple -enable-experimental-feature CalledAttribute -Onone %s -o %t/onone.out
// RUN: %target-build-swift -target %target-future-triple -enable-experimental-feature CalledAttribute -O -DOPTIMIZED %s -o %t/opt.out
// RUN: %target-codesign %t/onone.out %t/opt.out

// Each case that calls a closure runs to completion, and each case that drops
// one terminates the program. Dropping goes through
// withoutActuallyDiscarding, which is the only way to drop an exactly-once
// closure.
//
// RUN: %target-run %t/onone.out call | %FileCheck %s --check-prefix=CALL-call
// RUN: %target-run %t/opt.out call | %FileCheck %s --check-prefix=CALL-call
// RUN: %target-run %t/onone.out callWithResource | %FileCheck %s --check-prefix=CALL-callWithResource
// RUN: %target-run %t/opt.out callWithResource | %FileCheck %s --check-prefix=CALL-callWithResource
// RUN: %target-run %t/onone.out callEscaping | %FileCheck %s --check-prefix=CALL-callEscaping
// RUN: %target-run %t/opt.out callEscaping | %FileCheck %s --check-prefix=CALL-callEscaping
// RUN: %target-run %t/onone.out callGlobal | %FileCheck %s --check-prefix=CALL-callGlobal
// RUN: %target-run %t/opt.out callGlobal | %FileCheck %s --check-prefix=CALL-callGlobal
// RUN: %target-run %t/onone.out callCaptureless | %FileCheck %s --check-prefix=CALL-callCaptureless
// RUN: %target-run %t/opt.out callCaptureless | %FileCheck %s --check-prefix=CALL-callCaptureless
// RUN: %target-run %t/onone.out callBorrowed | %FileCheck %s --check-prefix=CALL-callBorrowed
// RUN: %target-run %t/opt.out callBorrowed | %FileCheck %s --check-prefix=CALL-callBorrowed
// RUN: %target-run %t/onone.out callMixed | %FileCheck %s --check-prefix=CALL-callMixed
// RUN: %target-run %t/opt.out callMixed | %FileCheck %s --check-prefix=CALL-callMixed
// RUN: %target-run %t/onone.out callGenericEscaping | %FileCheck %s --check-prefix=CALL-callGenericEscaping
// RUN: %target-run %t/opt.out callGenericEscaping | %FileCheck %s --check-prefix=CALL-callGenericEscaping
// RUN: %target-run %t/onone.out callAsync | %FileCheck %s --check-prefix=CALL-callAsync
// RUN: %target-run %t/opt.out callAsync | %FileCheck %s --check-prefix=CALL-callAsync
// FIXME: [called-once] The throwing case is compiled out at -O. See callThrowing.
// RUN: %target-run %t/onone.out callThrowing | %FileCheck %s --check-prefix=CALL-callThrowing
// RUN: %target-run %t/onone.out callGeneric | %FileCheck %s --check-prefix=CALL-callGeneric
// RUN: %target-run %t/opt.out callGeneric | %FileCheck %s --check-prefix=CALL-callGeneric
// RUN: %target-run %t/onone.out callGenericInout | %FileCheck %s --check-prefix=CALL-callGenericInout
// RUN: %target-run %t/opt.out callGenericInout | %FileCheck %s --check-prefix=CALL-callGenericInout
// RUN: %target-run %t/onone.out callGenericConsumingAndInout | %FileCheck %s --check-prefix=CALL-callGenericConsumingAndInout
// RUN: %target-run %t/opt.out callGenericConsumingAndInout | %FileCheck %s --check-prefix=CALL-callGenericConsumingAndInout
//
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/onone.out drop 2>&1 | %FileCheck %s --check-prefix=DROP
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/opt.out drop 2>&1 | %FileCheck %s --check-prefix=DROP
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/onone.out dropWithResource 2>&1 | %FileCheck %s --check-prefix=DROP
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/opt.out dropWithResource 2>&1 | %FileCheck %s --check-prefix=DROP
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/onone.out dropEscaping 2>&1 | %FileCheck %s --check-prefix=DROP
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/opt.out dropEscaping 2>&1 | %FileCheck %s --check-prefix=DROP
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/onone.out dropCaptureless 2>&1 | %FileCheck %s --check-prefix=DROP
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/opt.out dropCaptureless 2>&1 | %FileCheck %s --check-prefix=DROP
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/onone.out dropEscapingCaptureless 2>&1 | %FileCheck %s --check-prefix=DROP
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/opt.out dropEscapingCaptureless 2>&1 | %FileCheck %s --check-prefix=DROP
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/onone.out dropGlobal 2>&1 | %FileCheck %s --check-prefix=DROP
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/opt.out dropGlobal 2>&1 | %FileCheck %s --check-prefix=DROP
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/onone.out dropAfterCalls 2>&1 | %FileCheck %s --check-prefix=DROP
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/opt.out dropAfterCalls 2>&1 | %FileCheck %s --check-prefix=DROP
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/onone.out dropGeneric 2>&1 | %FileCheck %s --check-prefix=DROP
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/opt.out dropGeneric 2>&1 | %FileCheck %s --check-prefix=DROP
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/onone.out dropGenericEscaping 2>&1 | %FileCheck %s --check-prefix=DROP
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/opt.out dropGenericEscaping 2>&1 | %FileCheck %s --check-prefix=DROP
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/onone.out dropGenericInout 2>&1 | %FileCheck %s --check-prefix=DROP
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/opt.out dropGenericInout 2>&1 | %FileCheck %s --check-prefix=DROP
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/onone.out dropGenericConsumingAndInout 2>&1 | %FileCheck %s --check-prefix=DROP
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/opt.out dropGenericConsumingAndInout 2>&1 | %FileCheck %s --check-prefix=DROP
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/onone.out dropMixed 2>&1 | %FileCheck %s --check-prefix=DROP
// RUN: not --crash env %env-SWIFT_BACKTRACE=enable=no %target-run %t/opt.out dropMixed 2>&1 | %FileCheck %s --check-prefix=DROP

// REQUIRES: swift_feature_CalledAttribute
// REQUIRES: executable_test
// REQUIRES: concurrency

// The message comes from a runtime entry point that older, OS-resident runtimes
// don't have.
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// The simulator's launcher reports a crash as an ordinary failure, so
// `not --crash` can't tell that the program crashed.
// UNSUPPORTED: DARWIN_SIMULATOR={{.*}}

// DROP: Fatal error: Exactly-once closure was destroyed without being called

final class Box {
  var count = 0
}

struct Resource: ~Copyable {
  let name: String
  deinit { print("Resource \(name) deinit") }
}

@inline(never)
func call(_ f: @called(exactlyOnce) () -> Void) {
  f()
  print("called")
}

@inline(never)
func callEscaping(_ f: @escaping @called(exactlyOnce) () -> Void) {
  f()
  print("called")
}

@inline(never)
func drop(_ f: consuming @called(exactlyOnce) () -> Void) {
  withoutActuallyDiscarding(f) { _ in }
}

@inline(never)
func dropEscaping(_ f: consuming @escaping @called(exactlyOnce) () -> Void) {
  withoutActuallyDiscarding(f) { _ in }
}

@inline(never)
func hello() { print("hello") }

struct Generic<T>: ~Copyable {
  var value: T
  let name: String
  deinit { print("Generic \(name) deinit") }
  consuming func use() { print("Generic \(name) used") }
  mutating func mutate() { print("Generic \(name) mutated") }
}

@inline(never)
func callAsync(_ f: @escaping @called(exactlyOnce) () async -> Void) async {
  await f()
  print("called")
}

@inline(never)
func runMixed(_ consumed: consuming Resource, _ borrowed: consuming Resource) {
  // One capture is consumed by the call and the other is only borrowed, so
  // only the first may be destroyed by the closure.
  call {
    _ = consume consumed
    print(borrowed.name)
  }
}

@inline(never)
func runGeneric<T>(_ value: T, _ generic: consuming Generic<T>) {
  // The context owns an address-only capture, so the forwarder must move it
  // out of the context instead of copying it.
  callEscaping { _ = consume generic }
}

// A throwing exactly-once closure that consumes a capture and then throws must
// destroy the capture once and still count as called.
//
// FIXME: [called-once] This is compiled out when optimization is enabled
// because SILCombine asserts on a throwing closure of this shape at -O.
#if !OPTIMIZED
struct Failure: Error {}

@inline(never)
func callThrowing(_ f: @called(exactlyOnce) () throws -> Void) throws {
  try f()
  print("called")
}

@inline(never)
func runThrowing(_ resource: consuming Resource) {
  do {
    try callThrowing {
      _ = consume resource
      throw Failure()
    }
  } catch {
    print("caught")
  }
}
#endif

@inline(never)
func runAsync(_ resource: consuming Resource) async {
  await callAsync { _ = consume resource }
}

// A generic noncopyable value is address-only, so a closure that consumes it
// captures it indirectly. The closure's context owns the value, which is
// destroyed exactly once, and a closure that mutates it captures it inout, so
// the caller keeps ownership.
@inline(never)
func runGenericConsuming<T>(_ value: T, _ generic: consuming Generic<T>) {
  call { generic.use() }
}

@inline(never)
func dropGenericConsuming<T>(_ value: T, _ generic: consuming Generic<T>) {
  drop { generic.use() }
}

@inline(never)
func dropGenericConsumingEscaping<T>(_ value: T, _ generic: consuming Generic<T>) {
  dropEscaping { generic.use() }
}

@inline(never)
func runGenericInout<T>(_ value: T) {
  var generic = Generic(value: value, name: "inout")
  call { generic.mutate() }
  generic.use()
}

@inline(never)
func dropGenericInout<T>(_ value: T) {
  var generic = Generic(value: value, name: "inout")
  drop { generic.mutate() }
  generic.use()
}

// A closure that consumes one generic noncopyable value and mutates another
// owns only the consumed value.
@inline(never)
func runGenericConsumingAndInout<T>(_ value: T) {
  let consumed = Generic(value: value, name: "consumed")
  var mutated = Generic(value: value, name: "mutated")
  call {
    consumed.use()
    mutated.mutate()
  }
  mutated.use()
}

@inline(never)
func dropGenericConsumingAndInout<T>(_ value: T) {
  let consumed = Generic(value: value, name: "consumed")
  var mutated = Generic(value: value, name: "mutated")
  drop {
    consumed.use()
    mutated.mutate()
  }
  mutated.use()
}

@inline(never)
func dropMixed(_ consumed: consuming Resource, _ borrowed: consuming Resource) {
  drop {
    _ = consume consumed
    print(borrowed.name)
  }
}

@inline(never)
func run(_ which: String, _ box: Box, _ resource: consuming Resource) {
  switch which {
  case "call":
    call { box.count += 1 }
  case "callWithResource":
    call { _ = consume resource }
  case "callEscaping":
    callEscaping { box.count += 1 }
  case "callGlobal":
    callEscaping(hello)
  case "drop":
    drop { box.count += 1 }
  case "dropWithResource":
    drop { _ = consume resource }
  case "dropEscaping":
    dropEscaping { box.count += 1 }
  case "callCaptureless":
    call { print("body") }
  case "callBorrowed":
    // A stack-promoted closure doesn't own the noncopyable value that it
    // borrows, so calling the closure must not destroy that value.
    call { print(resource.name) }
  case "dropCaptureless":
    drop { print("never") }
  case "dropEscapingCaptureless":
    dropEscaping { print("never") }
  case "dropGlobal":
    dropEscaping(hello)
  case "dropAfterCalls":
    // Closures that share a context must not change the outcome of a later
    // drop.
    for _ in 0..<10 { callEscaping { print("many") } }
    dropEscaping { print("never") }
  default:
    fatalError("unknown case")
  }
}

switch CommandLine.arguments[1] {
case "callMixed":
  runMixed(Resource(name: "consumed"), Resource(name: "borrowed"))
case "callGenericEscaping":
  runGeneric(1, Generic(value: 1, name: "g"))
case "callAsync":
  await runAsync(Resource(name: "a"))
#if !OPTIMIZED
case "callThrowing":
  runThrowing(Resource(name: "t"))
#endif
case "callGeneric":
  runGenericConsuming(1, Generic(value: 1, name: "g"))
case "dropGeneric":
  dropGenericConsuming(1, Generic(value: 1, name: "g"))
case "dropGenericEscaping":
  dropGenericConsumingEscaping(1, Generic(value: 1, name: "g"))
case "callGenericInout":
  runGenericInout(1)
case "dropGenericInout":
  dropGenericInout(1)
case "callGenericConsumingAndInout":
  runGenericConsumingAndInout(1)
case "dropGenericConsumingAndInout":
  dropGenericConsumingAndInout(1)
case "dropMixed":
  dropMixed(Resource(name: "consumed"), Resource(name: "borrowed"))
default:
  run(CommandLine.arguments[1], Box(), Resource(name: "r"))
}

// CALL-call: called
// CALL-call-NEXT: Resource r deinit

// CALL-callWithResource: Resource r deinit
// CALL-callWithResource-NEXT: called

// CALL-callEscaping: called
// CALL-callEscaping-NEXT: Resource r deinit

// CALL-callGlobal: hello
// CALL-callGlobal-NEXT: called
// CALL-callGlobal-NEXT: Resource r deinit

// CALL-callCaptureless: body
// CALL-callCaptureless-NEXT: called

// CALL-callBorrowed: r
// CALL-callBorrowed-NEXT: called
// CALL-callBorrowed-NEXT: Resource r deinit
// CALL-callBorrowed-NOT: Resource r deinit

// CALL-callMixed: Resource consumed deinit
// CALL-callMixed-NEXT: borrowed
// CALL-callMixed-NEXT: called
// CALL-callMixed-NEXT: Resource borrowed deinit
// CALL-callMixed-NOT: deinit

// CALL-callGenericEscaping: Generic g deinit
// CALL-callGenericEscaping-NEXT: called
// CALL-callGenericEscaping-NOT: deinit

// CALL-callAsync: Resource a deinit
// CALL-callAsync-NEXT: called
// CALL-callAsync-NOT: deinit

// CALL-callGeneric: Generic g used
// CALL-callGeneric-NEXT: Generic g deinit
// CALL-callGeneric-NEXT: called
// CALL-callGeneric-NOT: deinit

// CALL-callGenericInout: Generic inout mutated
// CALL-callGenericInout-NEXT: called
// CALL-callGenericInout-NEXT: Generic inout used
// CALL-callGenericInout-NEXT: Generic inout deinit
// CALL-callGenericInout-NOT: deinit

// CALL-callGenericConsumingAndInout: Generic consumed used
// CALL-callGenericConsumingAndInout-NEXT: Generic consumed deinit
// CALL-callGenericConsumingAndInout-NEXT: Generic mutated mutated
// CALL-callGenericConsumingAndInout-NEXT: called
// CALL-callGenericConsumingAndInout-NEXT: Generic mutated used
// CALL-callGenericConsumingAndInout-NEXT: Generic mutated deinit
// CALL-callGenericConsumingAndInout-NOT: deinit

// CALL-callThrowing: Resource t deinit
// CALL-callThrowing-NEXT: caught
// CALL-callThrowing-NOT: deinit
