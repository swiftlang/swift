// RUN: %target-typecheck-verify-swift -disable-availability-checking -I %S/Inputs/has_symbol/

// REQUIRES: objc_interop

@_weakLinked import has_symbol_helper_objc

func testTypes() {
  if #_hasSymbol(ObjCClass.self) {}
  if #_hasSymbol(ObjCProtocol.self) {} // expected-error {{'#_hasSymbol' cannot be used with this declaration}}
}

func testClassMembers(_ c: ObjCClass) {
  if #_hasSymbol(c.classMemberProperty) {} // expected-error {{'#_hasSymbol' cannot be used with Objective-C property 'classMemberProperty'; use respondsToSelector() instead}}
  if #_hasSymbol(c.classMemberMethod) {} // expected-error {{'#_hasSymbol' cannot be used with Objective-C method 'classMemberMethod'; use respondsToSelector() instead}}

  if #_hasSymbol(c.directClassMemberProperty) {} // expected-error {{'#_hasSymbol' cannot be used with Objective-C property 'directClassMemberProperty'; use respondsToSelector() instead}}
  if #_hasSymbol(c.directClassMemberMethod) {} // expected-error {{'#_hasSymbol' cannot be used with Objective-C method 'directClassMemberMethod'; use respondsToSelector() instead}}
}

func testProtocolMembers(_ p: ObjCProtocol) {
  if #_hasSymbol(p.protocolMemberProperty) {} // expected-error {{'#_hasSymbol' cannot be used with Objective-C property 'protocolMemberProperty'; use respondsToSelector() instead}}
  if #_hasSymbol(p.protocolMemberMethod) {} // expected-error {{'#_hasSymbol' cannot be used with Objective-C method 'protocolMemberMethod'; use respondsToSelector() instead}}
}
