// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/has_symbol_helper.swiftmodule -parse-as-library %S/Inputs/has_symbol_helper.swift -enable-library-evolution
// RUN: %target-typecheck-verify-swift -disable-availability-checking -I %t

// UNSUPPORTED: OS=windows-msvc

@_weakLinked import has_symbol_helper

func testGlobalFunctions() {
  if #_hasSymbol(noArgFunc) {}
  if #_hasSymbol(function(with:)) {}
  if #_hasSymbol(throwingFunc) {}
  if #_hasSymbol(ambiguousFunc) {} // expected-error {{ambiguous use of 'ambiguousFunc()'}}
  if #_hasSymbol(ambiguousFunc as () -> Int) {}
  if #_hasSymbol(genericFunc(_:) as (S) -> Void) {}
}

func testVars() {
  if #_hasSymbol(global) {}
  if #_hasSymbol(global as Int) {}
}

func testStruct(_ s: S) {
  if #_hasSymbol(s.member) {}
  if #_hasSymbol(s.noArgsMethod) {}
  if #_hasSymbol(s.method(with:)) {}
  if #_hasSymbol(s.genericFunc(_:)) {} // expected-error {{generic parameter 'T' could not be inferred}}
  if #_hasSymbol(s.genericFunc(_:) as (S) -> ()) {}
  if #_hasSymbol(s.requirement) {}
  if #_hasSymbol(s.requirementWithDefaultImpl) {}

  if #_hasSymbol(S.staticFunc) {}
  if #_hasSymbol(S.staticMember) {}
  if #_hasSymbol(S.init(member:)) {}
}

func testGenericStruct(_ s: GenericS<S>) {
  if #_hasSymbol(s.member) {}
  if #_hasSymbol(s.noArgsMethod) {}
  if #_hasSymbol(s.method(with:)) {}
}

func testClass(_ c: C) {
  if #_hasSymbol(c.member) {}
  if #_hasSymbol(c.noArgsMethod) {}
  if #_hasSymbol(c.method(with:)) {}
  if #_hasSymbol(c.requirement) {}
  if #_hasSymbol(c.requirementWithDefaultImpl) {}

  if #_hasSymbol(C.classFunc) {}
  if #_hasSymbol(C.staticMember) {}
  if #_hasSymbol(C.init(member:)) {}
}

func testEnum(_ e: E) {
  if #_hasSymbol(E.basicCase) {}
  if #_hasSymbol(E.payloadCase) {}
  if #_hasSymbol(E.payloadCase(_:)) {}
  if #_hasSymbol(e.method) {}
}

func testMetatypes() {
  if #_hasSymbol(P.self) {}
  if #_hasSymbol(S.self) {}
  if #_hasSymbol(GenericS.self) {} // expected-error {{generic parameter 'T' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}}
  if #_hasSymbol(GenericS<S>.self) {}
  if #_hasSymbol(C.self) {}
  if #_hasSymbol(E.self) {}
}

var localGlobal: Int = 0

func localFunc() {}

struct LocalStruct {
  var member: Int = 0
}

protocol LocalProtocol {}

enum LocalEnum {
  case a
}

func testNotWeakDeclDiagnostics(_ s: LocalStruct) {
  if #_hasSymbol(localFunc) {} // expected-warning {{global function 'localFunc()' is not a weakly linked declaration}}
  if #_hasSymbol(localGlobal) {} // expected-warning {{var 'localGlobal' is not a weakly linked declaration}}
  if #_hasSymbol(s) {} // expected-warning {{parameter 's' is not a weakly linked declaration}}
  if #_hasSymbol(s.member) {} // expected-warning {{property 'member' is not a weakly linked declaration}}
  if #_hasSymbol(LocalEnum.a) {} // expected-warning {{enum case 'a' is not a weakly linked declaration}}
  if #_hasSymbol(LocalStruct.self) {} // expected-warning {{struct 'LocalStruct' is not a weakly linked declaration}}
  if #_hasSymbol(LocalProtocol.self) {} // expected-warning {{protocol 'LocalProtocol' is not a weakly linked declaration}}
}

func testInvalidExpressionsDiagnostics() {
  if #_hasSymbol(noArgFunc()) {} // expected-error {{#_hasSymbol condition must refer to a declaration}}
  if #_hasSymbol(global - 1) {} // expected-error {{#_hasSymbol condition must refer to a declaration}}
  if #_hasSymbol(S.staticFunc()) {} // expected-error {{#_hasSymbol condition must refer to a declaration}}
  if #_hasSymbol(C.classFunc()) {} // expected-error {{#_hasSymbol condition must refer to a declaration}}
  if #_hasSymbol(1 as Int) {} // expected-error {{#_hasSymbol condition must refer to a declaration}}
  if #_hasSymbol(1 as S) {} // expected-error {{cannot convert value of type 'Int' to type 'S' in coercion}}
}

func testMultiStatementClosure() {
  let _: () -> Void = { // expected-error {{unable to infer closure type in the current context}}
    if #_hasSymbol(global) {} // expected-error 2 {{#_hasSymbol is not supported in closures}}
  }
  
  let _: () -> Void = { // expected-error {{unable to infer closure type in the current context}}
    if #_hasSymbol(global) {} // expected-error 2 {{#_hasSymbol is not supported in closures}}
    localFunc()
  }
}

protocol View {}

@resultBuilder struct ViewBuilder {
  static func buildBlock<Content>(_ content: Content) -> Content where Content : View { fatalError() }
  static func buildEither<Content>(first content: Content) -> Content where Content : View { fatalError() }
  static func buildEither<Content>(second content: Content) -> Content where Content : View { fatalError() }
}

struct Image : View {
}

struct MyView {
  @ViewBuilder var body: some View {
    if #_hasSymbol(global) { // expected-error {{#_hasSymbol is not supported in closures}}
      Image()
    } else {
      Image()
    }
  }
}
