/// @c attribute
/// This test shouldn't require the objc runtime.

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s \
// RUN:   -enable-experimental-feature CDecl -disable-objc-interop

// REQUIRES: swift_feature_CDecl

@c(cdecl_foo) func foo(x: Int) -> Int { return x }

@c(not an identifier) func invalidName() {}
// expected-error @-1 {{expected ')' in 'c' attribute}}
// expected-error @-2 {{expected declaration}}

@c() func emptyParen() {}
// expected-error @-1 {{expected C identifier in 'c' attribute}}
// expected-error @-2 {{expected declaration}}

@c(42) func aNumber() {}
// expected-error @-1 {{expected C identifier in 'c' attribute}}
// expected-error @-2 {{expected declaration}}

@c(a:selector:) func selectordName() {}
// expected-error @-1 {{expected ')' in 'c' attribute}}
// expected-error @-2 {{expected declaration}}

@c func defaultName() {}

@c("noBody")
func noBody(x: Int) -> Int // expected-error{{expected '{' in body of function}}

@c("property") // expected-error{{'@c' attribute cannot be applied to this declaration}}
var property: Int

var computed: Int {
  @c("get_computed") get { return 0 }
  @c("set_computed") set { return }
}

@c("inout")
func noBody(x: inout Int) { } // expected-error{{global function cannot be marked '@c' because inout parameters cannot be represented in C}}

struct SwiftStruct { var x, y: Int }
enum SwiftEnum { case A, B }

#if os(Windows) && (arch(x86_64) || arch(arm64))
@c("CEnum") enum CEnum: Int32 { case A, B }
#else
@c("CEnum") enum CEnum: Int { case A, B }
#endif

@c enum CEnumDefaultName: CInt { case A, B }

@c("CEnumNoRawType") enum CEnumNoRawType { case A, B }
// expected-error @-1 {{'@c' enum must declare an integer raw type}}

@c("CEnumStringRawType") enum CEnumStringRawType: String { case A, B }
// expected-error @-1 {{'@c' enum raw type 'String' is not an integer type}}

@c("swiftStruct")
func swiftStruct(x: SwiftStruct) {}
// expected-error @-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{Swift structs cannot be represented in C}}

@c("swiftEnum")
func swiftEnum(x: SwiftEnum) {}
// expected-error @-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{Swift enums not marked '@c' cannot be represented in C}}

@c("cEnum")
func cEnum(x: CEnum) {}

@c("CDeclAndObjC") // expected-error {{cannot apply both '@c' and '@objc' to enum}}
@objc
enum CDeclAndObjC: CInt { case A, B }

@c("TwoCDecls") // expected-note {{attribute already specified here}}
@_cdecl("TwoCDecls") // expected-error {{duplicate attribute}}
func TwoCDecls() {}

class Foo {
  @c("Foo_foo") // expected-error{{@c can only be applied to global functions}}
  func foo(x: Int) -> Int { return x }

  @c("Foo_foo_2") // expected-error{{@c can only be applied to global functions}}
  static func foo(x: Int) -> Int { return x }

  @c("Foo_init") // expected-error{{'@c' attribute cannot be applied to this declaration}}
  init() {}

  @c("Foo_deinit") // expected-error{{'@c' attribute cannot be applied to this declaration}}
  deinit {}
}

@c("throwing") // expected-error{{raising errors from @c functions is not supported}}
func throwing() throws { }

@c("acceptedPointers")
func acceptedPointers(_ x: UnsafeMutablePointer<Int>,
                        y: UnsafePointer<Int>,
                        z: UnsafeMutableRawPointer,
                        w: UnsafeRawPointer,
                        u: OpaquePointer) {}

@c("rejectedPointers")
func rejectedPointers( // expected-error 6 {{global function cannot be marked '@c' because the type of the parameter}}
    x: UnsafePointer<String>, // expected-note {{Swift structs cannot be represented in C}}
    y: CVaListPointer, // expected-note {{Swift structs cannot be represented in C}}
    z: UnsafeBufferPointer<Int>, // expected-note {{Swift structs cannot be represented in C}}
    u: UnsafeMutableBufferPointer<Int>, // expected-note {{Swift structs cannot be represented in C}}
    v: UnsafeRawBufferPointer, // expected-note {{Swift structs cannot be represented in C}}
    t: UnsafeMutableRawBufferPointer) {} // expected-note {{Swift structs cannot be represented in C}}

@c("genericParam")
func genericParam<I>(i: I) {}
// expected-error @-1 {{global function cannot be marked '@c' because it has generic parameters}}

@c("variadic")
func variadic(_: Int...) {}
// expected-error @-1 {{global function cannot be marked '@c' because it has a variadic parameter}}

@c("tupleParamEmpty")
func tupleParamEmpty(a: ()) {}
// expected-error @-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{empty tuple type cannot be represented in C}}

@c("tupleParam")
func tupleParam(a: (Int, Float)) {}
// expected-error @-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{tuples cannot be represented in C}}

@c("emptyTupleReturn")
func emptyTupleReturn() -> () {}

@c("tupleReturn")
func tupleReturn() -> (Int, Float) { (1, 2.0) }
// expected-error @-1 {{global function cannot be marked '@c' because its result type cannot be represented in C}}
// expected-note @-2 {{tuples cannot be represented in C}}

@c("funcAcceptsThrowingFunc")
func funcAcceptsThrowingFunc(fn: (String) throws -> Int) { }
// expected-error @-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{throwing function types cannot be represented in C}}

@c("funcAcceptsThrowingFuncReturn")
func funcAcceptsThrowingFuncReturn() -> (String) throws -> Int { fatalError() }
// expected-error @-1 {{global function cannot be marked '@c' because its result type cannot be represented in C}}
// expected-note @-2 {{throwing function types cannot be represented in C}}

@c("bar")
func bar(f: (SwiftEnum) -> SwiftStruct) {}
// expected-error @-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{function types cannot be represented in C unless their parameters and returns can be}}

@c("bas")
func bas(f: (SwiftEnum) -> ()) {}
// expected-error @-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{function types cannot be represented in C unless their parameters and returns can be}}

@c("zim")
func zim(f: () -> SwiftStruct) {}
// expected-error @-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{function types cannot be represented in C unless their parameters and returns can be}}

@c("zang")
func zang(f: (SwiftEnum, SwiftStruct) -> ()) {}
// expected-error @-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{function types cannot be represented in C unless their parameters and returns can be}}

@c("zang_zang")
  func zangZang(f: (Int...) -> ()) {}
// expected-error @-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{function types cannot be represented in C unless their parameters and returns can be}}

@c("array")
func array(i: [Int]) {}
// expected-error @-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{Swift structs cannot be represented in C}}

class SwiftClass {}
@c("swiftClass")
func swiftClass(p: SwiftClass) {}
// expected-error @-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{classes cannot be represented in C}}

protocol SwiftProtocol {}
@c("swiftProtocol")
func swiftProtocol(p: SwiftProtocol) {}
// expected-error @-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{protocols cannot be represented in C}}

@c("swiftErrorProtocol")
func swiftErrorProtocol(e: Error) {}
// expected-error @-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{protocols cannot be represented in C}}

@c("anyParam")
func anyParam(e:Any) {}
// expected-error @-1 {{global function cannot be marked '@c' because the type of the parameter cannot be represented in C}}
// expected-note @-2 {{protocols cannot be represented in C}}

@c func swift_allocBox() {} // expected-warning {{symbol name 'swift_allocBox' is reserved for the Swift runtime and cannot be directly referenced without causing unpredictable behavior; this will become an error}}
@c(swift_allocObject) func swift_allocObject_renamed() {} // expected-warning {{symbol name 'swift_allocObject' is reserved for the Swift runtime and cannot be directly referenced without causing unpredictable behavior; this will become an error}}
