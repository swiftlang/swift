// RUN: %swift %clang-importer-sdk -parse %s -verify

import nullability;

func testSomeClass(sc: SomeClass, osc: SomeClass?) {
  var ao1: AnyObject = sc.methodA(osc)
  if sc.methodA(osc) == nil { } // expected-error{{cannot invoke}}

  var ao2: AnyObject = sc.methodB(nil)
  if sc.methodA(osc) == nil { } // expected-error{{cannot invoke}}

  var ao3: AnyObject = sc.property // expected-error{{value of optional type 'AnyObject?' not unwrapped; did you mean to use '!' or '?'?}}
  var ao3_ok: AnyObject? = sc.property // okay
}
