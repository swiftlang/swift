// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -dump-parse -disable-availability-checking -enable-experimental-move-only -enable-experimental-feature ParserASTGen > %t/astgen.ast.raw
// RUN: %target-swift-frontend %s -dump-parse -disable-availability-checking -enable-experimental-move-only > %t/cpp-parser.ast.raw

// Filter out any addresses in the dump, since they can differ.
// RUN: sed -E 's#0x[0-9a-fA-F]+##g' %t/cpp-parser.ast.raw > %t/cpp-parser.ast
// RUN: sed -E 's#0x[0-9a-fA-F]+##g' %t/astgen.ast.raw > %t/astgen.ast

// RUN: %diff -u %t/astgen.ast %t/cpp-parser.ast

// RUN: %target-typecheck-verify-swift -enable-experimental-move-only -enable-experimental-feature ParserASTGen 

// REQUIRES: executable_test
// REQUIRES: swift_swift_parser

// -enable-experimental-feature requires an asserts build
// REQUIRES: asserts
// rdar://116686158
// UNSUPPORTED: asan

struct S1 {
  static func staticMethod() {}
}

func testStatic() {
  // static.
  S1.staticMethod() 
  S1().staticMethod() // expected-error {{static member 'staticMethod' cannot be used on instance of type 'S1'}}
}

struct S2 {
  private func privateMethod() {} // expected-note {{'privateMethod()' declared here}}
  fileprivate func fileprivateMethod() {}
  internal func internalMethod() {}
  public func publicMethod() {}
}

func testAccessControl(value: S2) {
  // access control.
  value.privateMethod() // expected-error {{'privateMethod' is inaccessible due to 'private' protection level}}
  value.fileprivateMethod()
  value.internalMethod()
  value.publicMethod()
}

struct S3 {
  mutating func mutatingMethod() {}
  func normalMethod() {}
}

func testMutating(value: S3) {
  value.mutatingMethod() // expected-error {{cannot use mutating member on immutable value}}
  value.normalMethod()
}

@frozen // expected-error {{'@frozen' attribute cannot be applied to this declaration}}
class C1 {}
@_alignment(7) // expected-error {{alignment value must be a power of two}}
struct S4 {}

@implementation extension ObjCClass1 {} // expected-error {{cannot find type 'ObjCClass1' in scope}}
@implementation(Category) extension ObjCClass1 {} // expected-error {{cannot find type 'ObjCClass1' in scope}}
@_objcImplementation extension ObjCClass2 {} // expected-error {{cannot find type 'ObjCClass2' in scope}}
@_objcImplementation(Category) extension ObjCClass2 {} // expected-error {{cannot find type 'ObjCClass2' in scope}}
