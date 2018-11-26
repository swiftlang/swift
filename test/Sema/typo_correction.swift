// RUN: %target-typecheck-verify-swift -typo-correction-limit 20
// RUN: not %target-swift-frontend -typecheck -disable-typo-correction %s 2>&1 | %FileCheck %s -check-prefix=DISABLED
// RUN: not %target-swift-frontend -typecheck -typo-correction-limit 0 %s 2>&1 | %FileCheck %s -check-prefix=DISABLED
// RUN: not %target-swift-frontend -typecheck -DIMPORT_FAIL %s 2>&1 | %FileCheck %s -check-prefix=DISABLED
// DISABLED-NOT: did you mean

#if IMPORT_FAIL
import NoSuchModule
#endif

// This is close enough to get typo-correction.
func test_short_and_close() {
  let foo = 4 // expected-note {{'foo' declared here}}
  let bab = fob + 1
  // expected-error@-1 {{use of unresolved identifier 'fob'; did you mean 'foo'?}}
}

// This is not.
func test_too_different() {
  let moo = 4
  let bbb = mbb + 1 // expected-error {{use of unresolved identifier}}
}

struct Whatever {}
func *(x: Whatever, y: Whatever) {}

// This works even for single-character identifiers.
func test_very_short() {
  // Note that we don't suggest operators.
  let x = 0 // expected-note {{'x' declared here}}
  let longer = y
  // expected-error@-1 {{use of unresolved identifier 'y'; did you mean 'x'?}}
}

// It does not trigger in a variable's own initializer.
func test_own_initializer() {
  let x = y // expected-error {{use of unresolved identifier 'y'}}
}

// Report candidates that are the same distance in different ways.
func test_close_matches() {
  let match1 = 0 // expected-note {{did you mean 'match1'?}}
  let match22 = 0 // expected-note {{did you mean 'match22'?}}
  let x = match2 // expected-error {{use of unresolved identifier 'match2'}}
}

// Report not-as-good matches if they're still close enough to the best.
func test_keep_if_not_too_much_worse() {
  let longmatch1 = 0 // expected-note {{did you mean 'longmatch1'?}}
  let longmatch22 = 0 // expected-note {{did you mean 'longmatch22'?}}
  let x = longmatch // expected-error {{use of unresolved identifier 'longmatch'}}
}

// Report not-as-good matches if they're still close enough to the best.
func test_drop_if_too_different() {
  let longlongmatch1 = 0 // expected-note {{'longlongmatch1' declared here}}
  let longlongmatch2222 = 0
  let x = longlongmatch
  // expected-error@-1 {{use of unresolved identifier 'longlongmatch'; did you mean 'longlongmatch1'?}}
}

// Candidates are suppressed if we have too many that are the same distance.
func test_too_many_same() {
  let match1 = 0
  let match2 = 0
  let match3 = 0
  let match4 = 0
  let match5 = 0
  let match6 = 0
  let x = match // expected-error {{use of unresolved identifier 'match'}}
}

// But if some are better than others, just drop the worse tier.
func test_too_many_but_some_better() {
  let mtch1 = 0 // expected-note {{did you mean 'mtch1'?}}
  let mtch2 = 0 // expected-note {{did you mean 'mtch2'?}}
  let match3 = 0
  let match4 = 0
  let match5 = 0
  let match6 = 0
  let x = mtch // expected-error {{use of unresolved identifier 'mtch'}}
}

// rdar://problem/28387684
// Don't crash performing typo correction on bound generic types with
// type variables.
_ = [Any]().withUnsafeBufferPointer { (buf) -> [Any] in
  guard let base = buf.baseAddress else { return [] }
  return (base ..< base + buf.count).m // expected-error {{value of type 'Range<UnsafePointer<Any>>' has no member 'm'}}
}

// Typo correction with class-bound archetypes.
class SomeClass {
  func match1() {} // expected-note {{'match1' declared here}}
}

func takesSomeClassArchetype<T : SomeClass>(_ t: T) {
  t.match0()
  // expected-error@-1 {{value of type 'T' has no member 'match0'; did you mean 'match1'?}}{{5-11=match1}}
}

// Typo correction of unqualified lookup from generic context.
struct Generic<T> {
  func match1() {}
  // expected-note@-1 {{'match1' declared here}}

  class Inner {
    func doStuff() {
      match0()
      // expected-error@-1 {{use of unresolved identifier 'match0'; did you mean 'match1'?}}
    }
  }
}

protocol P { // expected-note {{'P' previously declared here}}
  typealias a = Generic
}

protocol P {} // expected-error {{invalid redeclaration of 'P'}}

func hasTypo() {
  _ = P.a.a // expected-error {{type 'P.a' (aka 'Generic') has no member 'a'}}
}

// Typo correction with AnyObject.
func takesAnyObject(_ t: AnyObject) {
  _ = t.rawPointer
  // expected-error@-1 {{value of type 'AnyObject' has no member 'rawPointer'}}
}

func takesAnyObjectArchetype<T : AnyObject>(_ t: T) {
  _ = t.rawPointer
  // expected-error@-1 {{value of type 'T' has no member 'rawPointer'}}
}

// Typo correction with an UnresolvedDotExpr.
enum Foo {
  case flashing // expected-note {{'flashing' declared here}}
}

func foo(_ a: Foo) {
}

func bar() {
  foo(.flashin)
  // expected-error@-1 {{type 'Foo' has no member 'flashin'; did you mean 'flashing'?}}{{8-15=flashing}}
}

// Verify that we emit a fixit even if there are multiple
// declarations with the corrected name.
func overloaded(_: Int) {} // expected-note {{'overloaded' declared here}}
func overloaded(_: Float) {} // expected-note {{'overloaded' declared here}}
func test_overloaded() {
  overloadd(0)
  // expected-error@-1 {{use of unresolved identifier 'overloadd'; did you mean 'overloaded'?}}{{3-12=overloaded}}
}

// This is one of the backtraces from rdar://36434823 but got fixed along
// the way.
class CircularValidationWithTypo {
  var cdcdcdcd = ababab { // expected-error {{use of unresolved identifier 'ababab'}}
    didSet { }
  }

  var abababab = cdcdcdc { // expected-error {{use of unresolved identifier 'cdcdcdc'}}
    didSet { }
  }
}

// Crash with invalid extension that has not been bound -- https://bugs.swift.org/browse/SR-8984
protocol PP {}

func boo() { // expected-note {{did you mean 'boo'?}}
  extension PP { // expected-error {{declaration is only valid at file scope}}
    func g() {
      booo() // expected-error {{use of unresolved identifier 'booo'}}
    }
  }
}
