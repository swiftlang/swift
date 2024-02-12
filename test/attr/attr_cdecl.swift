// RUN: %target-typecheck-verify-swift -enable-objc-interop

@_cdecl("cdecl_foo") func foo(x: Int) -> Int { return x }

@_cdecl("") // expected-error{{symbol name cannot be empty}}
func emptyName(x: Int) -> Int { return x }

@_cdecl("noBody")
func noBody(x: Int) -> Int // expected-error{{expected '{' in body of function}}

@_cdecl("property") // expected-error{{may only be used on 'func' declarations}}
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

@_cdecl("swiftStruct")
func swiftStruct(x: SwiftStruct) {} // expected-error{{cannot be represented}} expected-note{{Swift struct}}

@_cdecl("swiftEnum")
func swiftEnum(x: SwiftEnum) {} // expected-error{{cannot be represented}} expected-note{{non-'@objc' enum}}

@_cdecl("cEnum")
func cEnum(x: CEnum) {}

class Foo {
  @_cdecl("Foo_foo") // expected-error{{can only be applied to global functions}}
  func foo(x: Int) -> Int { return x }

  @_cdecl("Foo_foo_2") // expected-error{{can only be applied to global functions}}
  static func foo(x: Int) -> Int { return x }

  @_cdecl("Foo_init") // expected-error{{may only be used on 'func'}}
  init() {}

  @_cdecl("Foo_deinit") // expected-error{{may only be used on 'func'}}
  deinit {}
}

func hasNested() {
  @_cdecl("nested") // expected-error{{can only be used in a non-local scope}}
  func nested() { }
}

// TODO: Handle error conventions in SILGen for toplevel functions.
@_cdecl("throwing") // expected-error{{raising errors from @_cdecl functions is not supported}}
func throwing() throws { }

// TODO: cdecl name collisions
