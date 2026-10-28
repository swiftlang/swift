// RUN: %target-run-simple-swift(-target %target-future-triple -enable-experimental-feature NoncopyableCasting) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -target %target-future-triple -enable-experimental-feature NoncopyableCasting) | %FileCheck %s

// REQUIRES: swift_feature_NoncopyableCasting
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// An `is` test that the operand's static type already satisfies (an identity
// cast, or erasure to a protocol it conforms to) has nothing to ask the
// runtime. For a non-`Copyable` operand it used to be lowered as a consuming
// cast, so it was rejected on storage that can't be consumed -- a `borrowing`
// or `inout` parameter, a global, a class property -- although a runtime test
// of the same value was accepted. It now borrows the operand, evaluates it
// exactly once, and answers `true`. Both the expression and the `if case`
// pattern forms.

protocol P: ~Copyable {}

final class Canary {
  static var live = 0
  init() { Canary.live += 1 }
  deinit { Canary.live -= 1 }
}

struct Pay: P, ~Copyable { let canary = Canary(); var tag: Int }
final class Holder { var field = Pay(tag: 4) }

var global = Pay(tag: 5)
var evaluated = 0
func make() -> Pay { evaluated += 1; return Pay(tag: 6) }

func borrowedParam(_ x: borrowing Pay) -> Bool {
  x is Pay && x is any P & ~Copyable
}
func inoutParam(_ x: inout Pay) -> Bool {
  let result = x is Pay
  x.tag += 1
  return result
}
func borrowedExistential(_ x: borrowing any P & ~Copyable) -> Bool {
  x is any P & ~Copyable
}
func ifCasePattern(_ x: borrowing Pay) -> Bool {
  if case is Pay = x {
    if case is any P & ~Copyable = x { return true }
  }
  return false
}
func temporary() -> Bool { make() is Pay }

let local = Pay(tag: 1)
var mutable = Pay(tag: 2)
let holder = Holder()

// CHECK: borrowed: true true
print("borrowed:", borrowedParam(local), borrowedExistential(Pay(tag: 3)))
// CHECK: inout: true 3
print("inout:", inoutParam(&mutable), mutable.tag)
// CHECK: if case: true
print("if case:", ifCasePattern(local))
// CHECK: global and class property: true true
print("global and class property:", global is Pay, holder.field is Pay)
// CHECK: temporary: true evaluated 1
print("temporary:", temporary(), "evaluated", evaluated)

// The subjects are untouched, and nothing leaked: the four live values are
// `local`, `mutable`, `global` and `holder.field`.
// CHECK: after: 1 live 4
print("after:", local.tag, "live", Canary.live)
