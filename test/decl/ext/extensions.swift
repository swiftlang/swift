// RUN: %swift %s -verify

extension extension_for_invalid_type {  // expected-error {{use of undeclared type 'extension_for_invalid_type'}}
  static def f() { }
}

extension extension_for_invalid_type_2 {  // expected-error {{use of undeclared type 'extension_for_invalid_type_2'}}
  init() {}
}

extension Foo { // expected-error {{use of undeclared type 'Foo'}}
  typealias X = Int
}

//===--- Test that we only allow extensions at file scope.

extension NestingTest1 { // expected-error {{use of undeclared type 'NestingTest1'}}
  extension Foo {} // expected-error {{declaration is only valid at file scope}}
}
struct NestingTest2 {
  extension Foo {} // expected-error {{declaration is only valid at file scope}}
}
class NestingTest3 {
  extension Foo {} // expected-error {{declaration is only valid at file scope}}
}
enum NestingTest4 {
  extension Foo {} // expected-error {{declaration is only valid at file scope}}
}
protocol NestingTest5 {
  extension Foo {} // expected-error {{declaration is only valid at file scope}}
}
def nestingTest6() {
  extension Foo {} // expected-error {{declaration is only valid at file scope}}
}

//===--- Test that we only allow extensions only for nominal types.

// FIXME: This testcase should ideally not give an error.
extension String.GeneratorType {} // expected-error {{'GeneratorType' is not a member type of 'String'}}

struct S1 {}
extension S1 {} // no-error
extension S1.metatype {} // expected-error {{non-nominal type 'S1.metatype' cannot be extended}}
extension S1.metatype.metatype {} // expected-error {{non-nominal type 'S1.metatype.metatype' cannot be extended}}
extension (S1) {} // expected-error {{non-nominal type '(S1)' cannot be extended}}
extension ((S1)) {} // expected-error {{non-nominal type '((S1))' cannot be extended}}
extension S1? {} // expected-error {{non-nominal type 'S1?' cannot be extended}}

enum U1 {}
extension U1 {} // no-error
extension U1.metatype {} // expected-error {{non-nominal type 'U1.metatype' cannot be extended}}
extension U1.metatype.metatype {} // expected-error {{non-nominal type 'U1.metatype.metatype' cannot be extended}}
extension (U1) {} // expected-error {{non-nominal type '(U1)' cannot be extended}}
extension U1? {} // expected-error {{non-nominal type 'U1?' cannot be extended}}

class C1 {}
extension C1 {} // no-error
extension C1.metatype {} // expected-error {{non-nominal type 'C1.metatype' cannot be extended}}
extension C1.metatype.metatype {} // expected-error {{non-nominal type 'C1.metatype.metatype' cannot be extended}}
extension (C1) {} // expected-error {{non-nominal type '(C1)' cannot be extended}}
extension C1? {} // expected-error {{non-nominal type 'C1?' cannot be extended}}

protocol P1 {}
extension P1 {} // expected-error {{protocol 'P1' cannot be extended}}
extension P1.metatype {} // expected-error {{non-nominal type 'P1.metatype' cannot be extended}}
extension P1.metatype.metatype {} // expected-error {{non-nominal type 'P1.metatype.metatype' cannot be extended}}
extension (P1) {} // expected-error {{non-nominal type '(P1)' cannot be extended}}
extension P1? {} // expected-error {{non-nominal type 'P1?' cannot be extended}}

protocol P2 {}

extension () {} // expected-error {{non-nominal type '()' cannot be extended}}
extension (x: Int) {} // expected-error {{non-nominal type '(x : Int)' cannot be extended}}
extension (x: Int, y: Int) {} // expected-error {{non-nominal type '(x : Int, y : Int)' cannot be extended}}

typealias TupleAlias = (x: Int, y: Int)
extension TupleAlias {} // expected-error{{non-nominal type 'TupleAlias' cannot be extended}}

extension protocol<P1> {} // expected-error {{non-nominal type 'protocol<P1>' cannot be extended}}
extension protocol<P1, P2> {} // expected-error {{non-nominal type 'protocol<P1, P2>' cannot be extended}}

extension S1[] {} // expected-error {{non-nominal type 'S1[]' cannot be extended}}

extension S1[]? {} // expected-error {{non-nominal type 'S1[]?' cannot be extended}} expected-error {{optional array type requires parentheses}}

extension S1?[] {} // expected-error {{non-nominal type 'S1?[]' cannot be extended}}

extension (S1) -> () {} // expected-error {{non-nominal type '(S1) -> ()' cannot be extended}}

