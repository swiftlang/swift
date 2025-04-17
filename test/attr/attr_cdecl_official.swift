/// @cdecl attribute
/// This test shouldn't require the objc runtime.

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s \
// RUN:   -enable-experimental-feature CDecl -disable-objc-interop

// REQUIRES: swift_feature_CDecl

@cdecl(cdecl_foo) func foo(x: Int) -> Int { return x }

@cdecl(not an identifier) func invalidName() {}
// expected-error @-1 {{expected ')' in 'cdecl' attribute}}
// expected-error @-2 {{expected declaration}}

@cdecl() func emptyParen() {}
// expected-error @-1 {{expected C identifier in 'cdecl' attribute}}
// expected-error @-2 {{expected declaration}}

@cdecl(42) func aNumber() {}
// expected-error @-1 {{expected C identifier in 'cdecl' attribute}}
// expected-error @-2 {{expected declaration}}

@cdecl(a:selector:) func selectordName() {}
// expected-error @-1 {{expected ')' in 'cdecl' attribute}}
// expected-error @-2 {{expected declaration}}

@cdecl func defaultName() {}

@cdecl("noBody")
func noBody(x: Int) -> Int // expected-error{{expected '{' in body of function}}

@cdecl("property") // expected-error{{'@cdecl' attribute cannot be applied to this declaration}}
var property: Int

var computed: Int {
  @cdecl("get_computed") get { return 0 }
  @cdecl("set_computed") set { return }
}

@cdecl("inout")
func noBody(x: inout Int) { } // expected-error{{global function cannot be marked '@cdecl' because inout parameters cannot be represented in C}}

struct SwiftStruct { var x, y: Int }
enum SwiftEnum { case A, B }
#if os(Windows) && (arch(x86_64) || arch(arm64))
@objc enum CEnum: Int32 { case A, B }
#else
@objc enum CEnum: Int { case A, B }
#endif

@cdecl("swiftStruct")
func swiftStruct(x: SwiftStruct) {}
// expected-error @-1 {{global function cannot be marked '@cdecl' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{Swift structs cannot be represented in C}}

@cdecl("swiftEnum")
func swiftEnum(x: SwiftEnum) {} // expected-error{{cannot be represented}} expected-note{{non-'@objc' enum}}

@cdecl("cEnum")
func cEnum(x: CEnum) {}

class Foo {
  @cdecl("Foo_foo") // expected-error{{@cdecl can only be applied to global functions}}
  func foo(x: Int) -> Int { return x }

  @cdecl("Foo_foo_2") // expected-error{{@cdecl can only be applied to global functions}}
  static func foo(x: Int) -> Int { return x }

  @cdecl("Foo_init") // expected-error{{'@cdecl' attribute cannot be applied to this declaration}}
  init() {}

  @cdecl("Foo_deinit") // expected-error{{'@cdecl' attribute cannot be applied to this declaration}}
  deinit {}
}

@cdecl("throwing") // expected-error{{raising errors from @cdecl functions is not supported}}
func throwing() throws { }

@cdecl("acceptedPointers")
func acceptedPointers(_ x: UnsafeMutablePointer<Int>,
                        y: UnsafePointer<Int>,
                        z: UnsafeMutableRawPointer,
                        w: UnsafeRawPointer,
                        u: OpaquePointer) {}

@cdecl("rejectedPointers")
func rejectedPointers( // expected-error 6 {{global function cannot be marked '@cdecl' because the type of the parameter}}
    x: UnsafePointer<String>, // expected-note {{Swift structs cannot be represented in C}}
    y: CVaListPointer, // expected-note {{Swift structs cannot be represented in C}}
    z: UnsafeBufferPointer<Int>, // expected-note {{Swift structs cannot be represented in C}}
    u: UnsafeMutableBufferPointer<Int>, // expected-note {{Swift structs cannot be represented in C}}
    v: UnsafeRawBufferPointer, // expected-note {{Swift structs cannot be represented in C}}
    t: UnsafeMutableRawBufferPointer) {} // expected-note {{Swift structs cannot be represented in C}}

@cdecl("genericParam")
func genericParam<I>(i: I) {}
// expected-error @-1 {{global function cannot be marked '@cdecl' because it has generic parameters}}

@cdecl("variadic")
func variadic(_: Int...) {}
// expected-error @-1 {{global function cannot be marked '@cdecl' because it has a variadic parameter}}

@cdecl("tupleParamEmpty")
func tupleParamEmpty(a: ()) {}
// expected-error @-1 {{global function cannot be marked '@cdecl' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{empty tuple type cannot be represented in C}}

@cdecl("tupleParam")
func tupleParam(a: (Int, Float)) {}
// expected-error @-1 {{global function cannot be marked '@cdecl' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{tuples cannot be represented in C}}

@cdecl("emptyTupleReturn")
func emptyTupleReturn() -> () {}

@cdecl("tupleReturn")
func tupleReturn() -> (Int, Float) { (1, 2.0) }
// expected-error @-1 {{global function cannot be marked '@cdecl' because its result type cannot be represented in C}}
// expected-note @-2 {{tuples cannot be represented in C}}

@cdecl("funcAcceptsThrowingFunc")
func funcAcceptsThrowingFunc(fn: (String) throws -> Int) { }
// expected-error @-1 {{global function cannot be marked '@cdecl' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{throwing function types cannot be represented in C}}

@cdecl("funcAcceptsThrowingFuncReturn")
func funcAcceptsThrowingFuncReturn() -> (String) throws -> Int { fatalError() }
// expected-error @-1 {{global function cannot be marked '@cdecl' because its result type cannot be represented in C}}
// expected-note @-2 {{throwing function types cannot be represented in C}}

@cdecl("bar")
func bar(f: (SwiftEnum) -> SwiftStruct) {}
// expected-error @-1 {{global function cannot be marked '@cdecl' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{function types cannot be represented in C unless their parameters and returns can be}}

@cdecl("bas")
func bas(f: (SwiftEnum) -> ()) {}
// expected-error @-1 {{global function cannot be marked '@cdecl' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{function types cannot be represented in C unless their parameters and returns can be}}

@cdecl("zim")
func zim(f: () -> SwiftStruct) {}
// expected-error @-1 {{global function cannot be marked '@cdecl' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{function types cannot be represented in C unless their parameters and returns can be}}

@cdecl("zang")
func zang(f: (SwiftEnum, SwiftStruct) -> ()) {}
// expected-error @-1 {{global function cannot be marked '@cdecl' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{function types cannot be represented in C unless their parameters and returns can be}}

@cdecl("zang_zang")
  func zangZang(f: (Int...) -> ()) {}
// expected-error @-1 {{global function cannot be marked '@cdecl' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{function types cannot be represented in C unless their parameters and returns can be}}

@cdecl("array")
func array(i: [Int]) {}
// expected-error @-1 {{global function cannot be marked '@cdecl' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{Swift structs cannot be represented in C}}

class SwiftClass {}
@cdecl("swiftClass")
func swiftClass(p: SwiftClass) {}
// expected-error @-1 {{global function cannot be marked '@cdecl' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{classes cannot be represented in C}}

protocol SwiftProtocol {}
@cdecl("swiftProtocol")
func swiftProtocol(p: SwiftProtocol) {}
// expected-error @-1 {{global function cannot be marked '@cdecl' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{protocols cannot be represented in C}}

@cdecl("swiftErrorProtocol")
func swiftErrorProtocol(e: Error) {}
// expected-error @-1 {{global function cannot be marked '@cdecl' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{protocols cannot be represented in C}}

@cdecl("anyParam")
func anyParam(e:Any) {}
// expected-error @-1 {{global function cannot be marked '@cdecl' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{protocols cannot be represented in C}}
