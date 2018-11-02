// RUN: %target-typecheck-verify-swift

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
  deinit {} // expected-error {{deinitializers may only be declared within a class}}
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
extension S1.Type {} // expected-error {{cannot extend a metatype 'S1.Type'}}
extension S1.NestedStruct {} // no-error

struct S1_2 {
  // expected-error @+2 {{type member must not be named 'Type', since it would conflict with the 'foo.Type' expression}}
  // expected-note @+1 {{if this name is unavoidable, use backticks to escape it}} {{8-12=`Type`}}
  enum Type {}
}
struct S1_3 {
  enum `Type` {} // no-error
}

extension S1_2.Type {} // expected-error {{cannot extend a metatype 'S1_2.Type'}}
extension S1_3.`Type` {} // no-error

typealias TA_S1 = S1
extension TA_S1 {} // no-error

typealias TA_S1_NestedStruct = S1.NestedStruct
extension TA_S1_NestedStruct {} // no-error

enum U1 {
  struct NestedStruct {}
}
extension U1 {} // no-error
extension U1.NestedStruct {} // no-error

class C1 {
  struct NestedStruct {}
}
extension C1 {} // no-error
extension C1.NestedStruct {} // no-error

protocol P1 {}

protocol P2 {}

extension () {} // expected-error {{non-nominal type '()' cannot be extended}}

typealias TupleAlias = (x: Int, y: Int)
extension TupleAlias {} // expected-error{{non-nominal type 'TupleAlias' (aka '(x: Int, y: Int)') cannot be extended}}

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

protocol P3 {
  associatedtype Assoc
  func foo() -> Assoc
}

struct X3 : P3 {
}

extension X3.Assoc { // expected-error{{'Assoc' is not a member type of 'X3'}}
}

extension X3 {
  func foo() -> Int { return 0 }
}

// Make sure the test case from https://bugs.swift.org/browse/SR-3847 doesn't
// cause problems when the later extension is incorrectly nested inside another
// declaration.
extension C1.NestedStruct {
  static let originalValue = 0
}
struct WrapperContext {
  extension C1.NestedStruct { // expected-error {{declaration is only valid at file scope}}
    static let propUsingMember = originalValue // expected-error {{use of unresolved identifier 'originalValue'}}
  }
}
