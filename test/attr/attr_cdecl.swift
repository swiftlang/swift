// RUN: %target-typecheck-verify-swift -enable-objc-interop

@_cdecl("cdecl_foo") func foo(x: Int) -> Int { return x }

@_cdecl("") // expected-error{{symbol name cannot be empty}}
func emptyName(x: Int) -> Int { return x }

@_cdecl("noBody")
func noBody(x: Int) -> Int // expected-error{{expected '{' in body of function}}

@_cdecl("property") // expected-error{{'@_cdecl' attribute cannot be applied to this declaration}}
var property: Int

var computed: Int {
  @_cdecl("get_computed") get { return 0 }
  @_cdecl("set_computed") set { return }
}

struct SwiftStruct { var x, y: Int }
enum SwiftEnum { case A, B }
#if os(Windows) && (arch(x86_64) || arch(arm64))
@objc enum CEnum: Int32 { case A, B }
#else
@objc enum CEnum: Int { case A, B }
#endif

@_cdecl("enum") // expected-error {{@_cdecl may only be used on 'func' declarations}}
enum UnderscoreCDeclEnum: CInt { case A, B }

@_cdecl("swiftStruct")
func swiftStruct(x: SwiftStruct) {} // expected-error{{cannot be represented}} expected-note{{Swift struct}}

@_cdecl("swiftEnum")
func swiftEnum(x: SwiftEnum) {} // expected-error{{cannot be represented}} expected-note{{Swift enums not marked '@c' or '@objc' cannot be represented in Objective-C}}

@_cdecl("cEnum")
func cEnum(x: CEnum) {}

class Foo {
  @_cdecl("Foo_foo") // expected-error{{can only be applied to global functions}}
  func foo(x: Int) -> Int { return x }

  @_cdecl("Foo_foo_2") // expected-error{{can only be applied to global functions}}
  static func foo(x: Int) -> Int { return x }

  @_cdecl("Foo_init") // expected-error{{'@_cdecl' attribute cannot be applied to this declaration}}
  init() {}

  @_cdecl("Foo_deinit") // expected-error{{'@_cdecl' attribute cannot be applied to this declaration}}
  deinit {}
}

func hasNested() {
  @_cdecl("nested") // expected-error{{can only be used in a non-local scope}}
  func nested() { }
}

// TODO: Handle error conventions in SILGen for toplevel functions.
@_cdecl("throwing") // expected-error{{raising errors from @_cdecl functions is not supported}}
func throwing() throws { }

// Unicode.Scalar parameters are mapped to char32_t in the generated C header,
// but char32_t can hold values that are not valid Unicode scalars.
@_cdecl("takesScalar")
func takesScalar(x: Unicode.Scalar) {}
// expected-warning@-1{{'Unicode.Scalar' parameter 'x' will be exposed as 'char32_t'}}
// expected-note@-2{{use 'UInt32' and validate with 'Unicode.Scalar.init(_:)'}}{{21-35=UInt32}}

// Returning Unicode.Scalar is sound (every valid scalar fits in char32_t).
@_cdecl("returnsScalar")
func returnsScalar() -> Unicode.Scalar { fatalError() }

// TODO: cdecl name collisions
