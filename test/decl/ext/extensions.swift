// RUN: %swift %s -verify

extension extension_for_invalid_type_1 { // expected-error {{use of undeclared type 'extension_for_invalid_type_1'}}
  func f() { }
}
extension extension_for_invalid_type_2 { // expected-error {{use of undeclared type 'extension_for_invalid_type_2'}}
  static func f() { }
}
extension extension_for_invalid_type_3 { // expected-error {{use of undeclared type 'extension_for_invalid_type_3'}}
  init() {}
}
extension extension_for_invalid_type_4 { // expected-error {{use of undeclared type 'extension_for_invalid_type_4'}}
  deinit {} // expected-error {{'deinitializer' functions may only be declared within a class}}
}
extension extension_for_invalid_type_5 { // expected-error {{use of undeclared type 'extension_for_invalid_type_5'}}
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
func nestingTest6() {
  extension Foo {} // expected-error {{declaration is only valid at file scope}}
}

//===--- Test that we only allow extensions only for nominal types.

struct S1 {
  struct NestedStruct {}
}
extension S1 {} // no-error
extension S1.Type {} // expected-error {{non-nominal type 'S1.Type' cannot be extended}}
extension S1.Type.Type {} // expected-error {{non-nominal type 'S1.Type.Type' cannot be extended}}
extension (S1) {} // expected-error {{non-nominal type '(S1)' cannot be extended}}
extension ((S1)) {} // expected-error {{non-nominal type '((S1))' cannot be extended}}
extension S1? {} // expected-error {{non-nominal type 'S1?' cannot be extended}}
extension S1.NestedStruct {} // no-error

typealias TA_S1 = S1
extension TA_S1 {} // no-error

typealias TA_S1_NestedStruct = S1.NestedStruct
extension TA_S1_NestedStruct {} // no-error

enum U1 {
  struct NestedStruct {}
}
extension U1 {} // no-error
extension U1.Type {} // expected-error {{non-nominal type 'U1.Type' cannot be extended}}
extension U1.Type.Type {} // expected-error {{non-nominal type 'U1.Type.Type' cannot be extended}}
extension (U1) {} // expected-error {{non-nominal type '(U1)' cannot be extended}}
extension U1? {} // expected-error {{non-nominal type 'U1?' cannot be extended}}
extension U1.NestedStruct {} // no-error

class C1 {
  struct NestedStruct {}
}
extension C1 {} // no-error
extension C1.Type {} // expected-error {{non-nominal type 'C1.Type' cannot be extended}}
extension C1.Type.Type {} // expected-error {{non-nominal type 'C1.Type.Type' cannot be extended}}
extension (C1) {} // expected-error {{non-nominal type '(C1)' cannot be extended}}
extension C1? {} // expected-error {{non-nominal type 'C1?' cannot be extended}}
extension C1.NestedStruct {} // no-error

protocol P1 {}
extension P1 {} // expected-error {{protocol 'P1' cannot be extended}}
extension P1.Type {} // expected-error {{non-nominal type 'P1.Type' cannot be extended}}
extension P1.Type.Type {} // expected-error {{non-nominal type 'P1.Type.Type' cannot be extended}}
extension (P1) {} // expected-error {{non-nominal type '(P1)' cannot be extended}}
extension P1? {} // expected-error {{non-nominal type 'P1?' cannot be extended}}

protocol P2 {}

extension () {} // expected-error {{non-nominal type '()' cannot be extended}}
extension (x: Int) {} // expected-error {{non-nominal type '(x: Int)' cannot be extended}}
extension (x: Int, y: Int) {} // expected-error {{non-nominal type '(x: Int, y: Int)' cannot be extended}}

typealias TupleAlias = (x: Int, y: Int)
extension TupleAlias {} // expected-error{{non-nominal type 'TupleAlias' cannot be extended}}

extension protocol<P1> {} // expected-error {{non-nominal type 'protocol<P1>' cannot be extended}}
extension protocol<P1, P2> {} // expected-error {{non-nominal type 'protocol<P1, P2>' cannot be extended}}
// For non-nominal types, we also need to properly recover from errors when continuing with semantic analysis
extension protocol<P1> { // expected-error {{non-nominal type 'protocol<P1>' cannot be extended}}
  init() {
  }
}

extension S1[] {} // expected-error {{non-nominal type 'S1[]' cannot be extended}}

extension S1[]? {} // expected-error {{non-nominal type 'S1[]?' cannot be extended}} expected-error {{optional array type requires parentheses}}

extension S1?[] {} // expected-error {{non-nominal type 'S1?[]' cannot be extended}}

extension (S1) -> () {} // expected-error {{non-nominal type '(S1) -> ()' cannot be extended}}

// Test property accessors in extended types
class C {}
extension C {
  var p1: Int {
    get {return 1}
    set(v) {}
  }
}
var c = C()
var x = c.p1
c.p1 = 1

