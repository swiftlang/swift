// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/has_symbol_helper.swiftmodule -parse-as-library %S/Inputs/has_symbol/has_symbol_helper.swift -enable-library-evolution -target %target-swift-5.1-abi-triple
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -I %t -target %target-swift-5.1-abi-triple -swift-version 5
// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -I %t -target %target-swift-5.1-abi-triple -swift-version 6

// UNSUPPORTED: OS=windows-msvc

@_weakLinked import has_symbol_helper

func testGlobalFunctions() {
  if #_hasSymbol(noArgFunc) {}
  if #_hasSymbol(noArgFunc as () throws -> Void) {}
  if #_hasSymbol(noArgFunc as () async -> Void) {}
  if #_hasSymbol(function(with:)) {}
  if #_hasSymbol(throwingFunc) {}
  if #_hasSymbol(ambiguousFunc) {} // expected-error {{ambiguous use of 'ambiguousFunc()'}}
  if #_hasSymbol(ambiguousFunc as () -> Int) {}
  if #_hasSymbol(genericFunc(_:) as (S) -> Void) {}
  if #_hasSymbol(mainActorFunc) {}
  if #_hasSymbol(mainActorFunction(with:)) {}
}

func testVars() {
  if #_hasSymbol(global) {}
  if #_hasSymbol(global as Int) {}
}

func testStruct(_ s: S) {
  if #_hasSymbol(s.member) {}
  if #_hasSymbol(s.noArgsMethod) {}
  if #_hasSymbol(s.noArgsMethod as () throws -> Void) {}
  if #_hasSymbol(s.noArgsMethod as () async -> Void) {}
  if #_hasSymbol(S.noArgsMethod) {}
  if #_hasSymbol(S.noArgsMethod as (S) -> () throws -> Void) {}
  if #_hasSymbol(s.method(with:)) {}
  if #_hasSymbol(s.method(with:) as (Int) -> Void) {}
  if #_hasSymbol(S.method(with:)) {}
  if #_hasSymbol(s.ambiguousMethod) {} // expected-error {{ambiguous use of 'ambiguousMethod()'}}
  if #_hasSymbol(s.ambiguousMethod as () -> Int) {}
  if #_hasSymbol(s.ambiguousMethod as () throws -> Int) {}
  if #_hasSymbol(S.ambiguousMethod as (S) -> () -> Int) {}
  if #_hasSymbol(s.genericFunc(_:)) {} // expected-error {{generic parameter 'T' could not be inferred}}
  if #_hasSymbol(s.genericFunc(_:) as (S) -> ()) {}
  if #_hasSymbol(s.requirement) {}
  if #_hasSymbol(S.requirement) {}
  if #_hasSymbol(s.requirementWithDefaultImpl) {}
  if #_hasSymbol(s.mainActorMethod) {}
  if #_hasSymbol(S.mainActorMethod) {}
  if #_hasSymbol(S.mainActorStaticFunc) {}
  if #_hasSymbol(S.staticFunc) {}
  if #_hasSymbol(S.staticMember) {}
  if #_hasSymbol(S.init(member:)) {}
}

func testGenericStruct(_ s: GenericS<S>) {
  if #_hasSymbol(s.member) {}
  if #_hasSymbol(s.noArgsMethod) {}
  if #_hasSymbol(GenericS<S>.noArgsMethod) {}
  if #_hasSymbol(s.method(with:)) {}
}

func testClass(_ c: C, _ sub: SubC) {
  if #_hasSymbol(c.member) {}
  if #_hasSymbol(c.noArgsMethod) {}
  if #_hasSymbol(C.noArgsMethod) {}
  if #_hasSymbol(c.method(with:)) {}
  if #_hasSymbol(c.requirement) {}
  if #_hasSymbol(c.requirementWithDefaultImpl) {}
  if #_hasSymbol(c.mainActorMethod) {}
  if #_hasSymbol(C.mainActorMethod) {}
  if #_hasSymbol(C.classFunc) {}
  if #_hasSymbol(C.staticMember) {}
  if #_hasSymbol(C.init(member:)) {}
  if #_hasSymbol(c.methodReturningSelf) {}
  if #_hasSymbol(C.methodReturningSelf) {}

  if #_hasSymbol(sub.mainActorMethodReturningSelf) {}
  if #_hasSymbol(sub.methodReturningSelf) {}
  if #_hasSymbol(sub.methodReturningSelf as () -> SubC) {}
  if #_hasSymbol(sub.methodReturningSelf as () -> any P) {}
  if #_hasSymbol(SubC.methodReturningSelf) {}
}

func testEnum(_ e: E) {
  if #_hasSymbol(E.basicCase) {}
  if #_hasSymbol(E.payloadCase) {}
  if #_hasSymbol(E.payloadCase(_:)) {}
  if #_hasSymbol(e.method) {}
  if #_hasSymbol(E.method) {}
}

extension S {
  func testImplicitSelf() {
    if #_hasSymbol(noArgsMethod) {}
    if #_hasSymbol(noArgsMethod as () throws -> Void) {}
    if #_hasSymbol(noArgsMethod as () async -> Void) {}
    if #_hasSymbol(method(with:) as (Int) -> Void) {}
    if #_hasSymbol(ambiguousMethod) {} // expected-error {{ambiguous use of 'ambiguousMethod()'}}
    if #_hasSymbol(ambiguousMethod as () -> Int) {}
  }
}

@MainActor
func testFromActorIsolatedContext(_ s: S) {
  if #_hasSymbol(mainActorFunc) {}
  if #_hasSymbol(s.mainActorMethod) {}
  if #_hasSymbol(s.mainActorMember) {}
  if #_hasSymbol(S.mainActorStaticMember) {}
  if #_hasSymbol(s[0]) {}
}

func testOpaqueParameter<T: PAT>(_ p: T) {
  // FIXME: Improve this diagnostic
  if #_hasSymbol(T.A.self) {} // expected-error {{'#_hasSymbol' condition must refer to a declaration}}
  if #_hasSymbol(p.requirement) {}
  if #_hasSymbol(p.requirementWithDefaultImpl) {}
}

func testExistentialParameter(_ p: any P) {
  if #_hasSymbol(p.requirement) {}
  if #_hasSymbol(p.requirementWithDefaultImpl) {}

  // An uncurried reference to a protocol requirement.
  if #_hasSymbol(P.requirement) {}
  if #_hasSymbol(P.requirementWithDefaultImpl) {}
}

func testMetatypes() {
  if #_hasSymbol(P.self) {}
  if #_hasSymbol(S.self) {}
  if #_hasSymbol(GenericS.self) {} // expected-error {{generic parameter 'T' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}}
  if #_hasSymbol(GenericS<S>.self) {}
  if #_hasSymbol(C.self) {}
  if #_hasSymbol(E.self) {}
}

nonisolated(unsafe) var localGlobal: Int = 0

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
  if #_hasSymbol(unknownDecl) {} // expected-error {{cannot find 'unknownDecl' in scope}}
  if #_hasSymbol(noArgFunc()) {} // expected-error {{'#_hasSymbol' condition must refer to a declaration}}
  if #_hasSymbol(global - 1) {} // expected-error {{'#_hasSymbol' condition must refer to a declaration}}
  if #_hasSymbol(S.staticFunc()) {} // expected-error {{'#_hasSymbol' condition must refer to a declaration}}
  if #_hasSymbol(C.classFunc()) {} // expected-error {{'#_hasSymbol' condition must refer to a declaration}}
  if #_hasSymbol(1 as Int) {} // expected-error {{'#_hasSymbol' condition must refer to a declaration}}
  if #_hasSymbol(1 as S) {} // expected-error {{cannot convert value of type 'Int' to type 'S' in coercion}}
}

func testGuard() {
  guard #_hasSymbol(global) else { return }
  guard #_hasSymbol(unknownDecl) else { return } // expected-error {{cannot find 'unknownDecl' in scope}}
  guard #_hasSymbol(localFunc) else { return } // expected-warning {{global function 'localFunc()' is not a weakly linked declaration}}
}

func testWhile() {
  while #_hasSymbol(global) { break }
  while #_hasSymbol(unknownDecl) { break } // expected-error {{cannot find 'unknownDecl' in scope}}
  while #_hasSymbol(localFunc) { break } // expected-warning {{global function 'localFunc()' is not a weakly linked declaration}}
}

@usableFromInline
func doIt(_ closure: () -> ()) {
  closure()
}

@inlinable
func testInlinable() {
  if #_hasSymbol(noArgFunc) {}
  doIt {
    if #_hasSymbol(noArgFunc) {}
  }
}

@_alwaysEmitIntoClient
func testAEIC() {
  if #_hasSymbol(noArgFunc) {}
}

func testClosure() {
  doIt { if #_hasSymbol(global) {} }
  doIt { if #_hasSymbol(noArgFunc) {} }
  doIt { if #_hasSymbol(ambiguousFunc as () -> Int) {} }
  doIt { if #_hasSymbol(S.self) {} }
  doIt { if #_hasSymbol(ambiguousFunc) {} } // expected-error {{ambiguous use of 'ambiguousFunc()'}}
  doIt { if #_hasSymbol(localFunc) {} } // expected-warning {{global function 'localFunc()' is not a weakly linked declaration}}
  doIt { if #_hasSymbol(unknownDecl) {} } // expected-error {{cannot find 'unknownDecl' in scope}}
  doIt { if #_hasSymbol(noArgFunc()) {} } // expected-error {{'#_hasSymbol' condition must refer to a declaration}}
  doIt { if #_hasSymbol(global - 1) {} } // expected-error {{'#_hasSymbol' condition must refer to a declaration}}
  doIt { if #_hasSymbol(S.staticFunc()) {} } // expected-error {{'#_hasSymbol' condition must refer to a declaration}}
  doIt { if #_hasSymbol(C.classFunc()) {} } // expected-error {{'#_hasSymbol' condition must refer to a declaration}}
  doIt { if #_hasSymbol(1 as Int) {} } // expected-error {{'#_hasSymbol' condition must refer to a declaration}}
  doIt { if #_hasSymbol(1 as S) {} } // expected-error {{cannot convert value of type 'Int' to type 'S' in coercion}}
}

protocol View {}

@resultBuilder struct ViewBuilder {
  static func buildBlock<Content>(_ content: Content) -> Content where Content : View { fatalError() }
  static func buildEither<Content>(first content: Content) -> Content where Content : View { fatalError() }
  static func buildEither<Content>(second content: Content) -> Content where Content : View { fatalError() }
}

struct Image : View {}

struct MyView {
  let image = Image()
  
  @ViewBuilder var globalView: some View {
    if #_hasSymbol(global) { image }
    else { image }
  }
  
  @ViewBuilder var ambiguousFuncView: some View {
    if #_hasSymbol(ambiguousFunc) { image } // expected-error {{ambiguous use of 'ambiguousFunc()'}}
    else { image }
  }
    
  @ViewBuilder var localFuncView: some View {
    if #_hasSymbol(localFunc) { image } // expected-warning {{global function 'localFunc()' is not a weakly linked declaration}}
    else { image }
  }

  @ViewBuilder var noArgFuncView: some View {
    if #_hasSymbol(noArgFunc()) { image } // expected-error {{'#_hasSymbol' condition must refer to a declaration}}
    else { image }
  }
}
