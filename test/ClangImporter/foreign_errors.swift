// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -parse-as-library -verify %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -O -parse-as-library -DEMIT_SIL %s

// REQUIRES: objc_interop

import Foundation
import errors

#if !EMIT_SIL
func test0() {
  try ErrorProne.fail() // expected-error {{errors thrown from here are not handled}}
}
#endif

// Test "AndReturnError" stripping.
// rdar://20722195
func testAndReturnError() throws {
  try ErrorProne.fail()
  try ErrorProne.go()
  try ErrorProne.tryAndReturnError(()) // collides with 'try' keyword

  ErrorProne.messUpSignatureAndReturnError(nil) // wrong signature
}

func testInheritedInit() throws {
  try ReallyErrorProne(one: nil) // expected-warning{{unused}}
}

func testInheritedFactory() throws {
  try ReallyErrorProne(two: nil) // expected-warning{{unused}}
}

// Resolve a conflict between -foo and -foo: by just not
// importing the latter as throwing.
func testConflict1(_ obj: ErrorProne) throws {
  try obj.conflict1() // expected-warning {{no calls to throwing functions occur within 'try'}}
}
func testConflict1_error(_ obj: ErrorProne) throws {
  var error: NSError?
  obj.conflict1(&error)
}

// Resolve a conflict between -foo and -fooAndReturnError:
// by not changing the name of the latter.
func testConflict2(_ obj: ErrorProne) throws {
  try obj.conflict2() // expected-warning {{no calls to throwing functions occur within 'try'}}
}
func testConflict2_error(_ obj: ErrorProne) throws {
  try obj.conflict2AndReturnError()
}

// Resolve a conflict between -foo: and -foo:error: by not
// changing the name of the latter.
func testConflict3(_ obj: ErrorProne) throws {
  try obj.conflict3(nil) // expected-warning {{no calls to throwing functions occur within 'try'}}
}
func testConflict3_error(_ obj: ErrorProne) throws {
  try obj.conflict3(nil, error: ())
}

// Same as above but with an initializer.
// <rdar://problem/20922973>
func testConflict4() throws {
  try ErrorProne(newtonMessagePad: "Dilbert") // expected-warning {{no calls to throwing functions occur within 'try'}} // expected-warning{{unused}}
}

func testConflict4_error() throws {
  try ErrorProne(newtonMessagePad: "Eat Up Martha", error: ()) // expected-warning{{unused}}
}

func testBlockFinal() throws {
  try ErrorProne.run(callback: {})
  try ErrorProne.runWithAnError(callback: {})
  try ErrorProne.runSwiftly(5000, callback: {})
}

#if !EMIT_SIL
func testNonBlockFinal() throws {
  ErrorProne.runWithError(count: 0) // expected-error {{missing argument for parameter #1 in call}}
  // TODO(diagnostics): For situations where both label and type where incorrect, we should produce a single error
  // which would say something like `cannot invoke 'bar' with argument list (count: Int)`.
  ErrorProne.run(count: 0) // expected-error {{incorrect argument label in call (have 'count:', expected 'callback:')}}
  // expected-error@-1 {{cannot convert value of type 'Int' to expected argument type '(() -> Void)?'}}
}
#endif

class VeryErrorProne : ErrorProne {
  override class func fail() throws {}
}

func testConflictWithUnavailable() throws {
  try ErrorProne.doTheThing(42)
}

// rdar://21715350
func testSwiftError() throws {
  var err: NSError?
  let _: Bool = try ErrorProne.bound()
  let _: Float = try ErrorProne.bounce()
  let _: () = try ErrorProne.flounce()
  let _: CInt = try ErrorProne.ounce()
  let _: () = try ErrorProne.once()
  let _: () = try ErrorProne.sconce()
  let _: () = try ErrorProne.scotch()
  let _: Bool = ErrorProne.scout(&err)
}

// rdar://21074857
func needsNonThrowing(_ fn: () -> Void) {}
func testNSErrorExhaustive() {
  needsNonThrowing {
    do {
      try ErrorProne.fail()
    } catch let e as NSError {
      e // expected-warning {{expression of type 'NSError' is unused}}
    }
  }
}

func testBadOverrides(obj: FoolishErrorSub) throws {
  try obj.performRiskyOperation()
  let _: FoolishErrorSub = try obj.produceRiskyOutput()
  let _: String = try obj.produceRiskyString()

  let _: NSObject = try obj.badNullResult()
  let _: CInt = try obj.badNullResult2() // This is unfortunate but consistent.
  let _: CInt = try obj.badZeroResult()
  try obj.badNonzeroResult() as Void

  let base = obj as SensibleErrorBase
  try base.performRiskyOperation()
  let _: NSObject = try base.produceRiskyOutput()
  let _: String = try base.produceRiskyString()

  let _: NSObject = try base.badNullResult()
  let _: NSObject = try base.badNullResult2()
  let _: CInt = try base.badZeroResult()
  try base.badNonzeroResult() as Void
}
