// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s \
// RUN:   -enable-experimental-feature CDecl -disable-objc-interop

// REQUIRES: swift_feature_CDecl

@cdecl("cdecl_foo") func foo(x: Int) -> Int { return x }

@cdecl("") // expected-error{{@cdecl symbol name cannot be empty}}
func emptyName(x: Int) -> Int { return x }

@cdecl("noBody")
func noBody(x: Int) -> Int // expected-error{{expected '{' in body of function}}

@cdecl("property") // expected-error{{may only be used on 'func' declarations}}
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
func swiftStruct(x: SwiftStruct) {} // expected-error{{cannot be represented}} expected-note{{Swift struct}}

@cdecl("swiftEnum")
func swiftEnum(x: SwiftEnum) {} // expected-error{{cannot be represented}} expected-note{{non-'@objc' enum}}

@cdecl("cEnum")
func cEnum(x: CEnum) {}

class Foo {
  @cdecl("Foo_foo") // expected-error{{@cdecl can only be applied to global functions}}
  func foo(x: Int) -> Int { return x }

  @cdecl("Foo_foo_2") // expected-error{{@cdecl can only be applied to global functions}}
  static func foo(x: Int) -> Int { return x }

  @cdecl("Foo_init") // expected-error{{@cdecl may only be used on 'func'}}
  init() {}

  @cdecl("Foo_deinit") // expected-error{{@cdecl may only be used on 'func'}}
  deinit {}
}

@cdecl("throwing") // expected-error{{raising errors from @cdecl functions is not supported}}
func throwing() throws { }
