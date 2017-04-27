// RUN: %target-typecheck-verify-swift
// RUN: not %target-swift-frontend -typecheck -disable-typo-correction %s 2>&1 | %FileCheck %s -check-prefix=DISABLED
// RUN: not %target-swift-frontend -typecheck -DIMPORT_FAIL %s 2>&1 | %FileCheck %s -check-prefix=DISABLED
// DISABLED-NOT: did you mean

#if IMPORT_FAIL
import NoSuchModule
#endif

// This is close enough to get typo-correction.
func test_short_and_close() {
  let foo = 4 // expected-note {{did you mean 'foo'?}}
  let bab = fob + 1 // expected-error {{use of unresolved identifier}}
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
  let x = 0 // expected-note {{did you mean 'x'?}}
  let longer = y // expected-error {{use of unresolved identifier 'y'}}
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
  let longlongmatch1 = 0 // expected-note {{did you mean 'longlongmatch1'?}}
  let longlongmatch2222 = 0
  let x = longlongmatch // expected-error {{use of unresolved identifier 'longlongmatch'}}
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
  return (base ..< base + buf.count).m // expected-error {{value of type 'CountableRange<_>' has no member 'm'}}
}

// Typo correction with class-bound archetypes.
class SomeClass {
  func match1() {}
  // expected-note@-1 {{did you mean 'match1'?}}
}

func takesSomeClassArchetype<T : SomeClass>(_ t: T) {
  t.match0()
  // expected-error@-1 {{value of type 'T' has no member 'match0'}}
}

// Typo correction of unqualified lookup from generic context.
struct Generic<T> {
  func match1() {}
  // expected-note@-1 {{did you mean 'match1'?}}

  class Inner {
    func doStuff() {
      match0()
      // expected-error@-1 {{use of unresolved identifier 'match0'}}
    }
  }
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
