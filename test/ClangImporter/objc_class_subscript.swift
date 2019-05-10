// RUN: %empty-directory(%t)
// RUN: %build-clang-importer-objc-overlays

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -typecheck -I %S/Inputs/custom-modules %s -verify

// REQUIRES: objc_interop

import ObjCSubscripts

func testClass() {
  _ = NoClassSubscript[0] // expected-error{{value of type 'NoClassSubscript.Type' has no subscripts}}
  NoClassSubscript[0] = "" // expected-error{{value of type 'NoClassSubscript.Type' has no subscripts}}
  
  _ = NoClassSubscript["foo"] // expected-error{{value of type 'NoClassSubscript.Type' has no subscripts}}
  NoClassSubscript["foo"] = "" // expected-error{{value of type 'NoClassSubscript.Type' has no subscripts}}
}

func testInstance(x: NoClassSubscript) {
  _ = x[0] // expected-error{{value of type 'NoClassSubscript' has no subscripts}}
  x[0] = "" // expected-error{{value of type 'NoClassSubscript' has no subscripts}}
  
  _ = x["foo"] // expected-error{{value of type 'NoClassSubscript' has no subscripts}}
  x["foo"] = "" // expected-error{{value of type 'NoClassSubscript' has no subscripts}}
}

func testClassMethods() {
  _ = NoClassSubscript.object(atIndexedSubscript: 0)
  NoClassSubscript.setObject("", atIndexedSubscript: 0)
  
  _ = NoClassSubscript.object(forKeyedSubscript: "foo")
  NoClassSubscript.setObject("", forKeyedSubscript: "foo")
}
