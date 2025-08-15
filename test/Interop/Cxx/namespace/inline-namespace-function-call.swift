// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -c -verify -verify-ignore-unknown -I %t/Inputs  %t/test.swift  -enable-experimental-cxx-interop -verify

//--- Inputs/module.modulemap
module namespaces {
  header "test.h"
  requires cplusplus
}
//--- Inputs/test.h
namespace Parent {
inline namespace InlineChild {
inline namespace InlineChild2 {
void functionInInlineChild2();
} // namespace InlineChild2
void functionInInlineChild();
namespace Child {
void functionInInlineChildsNonInlineNamespace();
} // namespace Child
} // namespace InlineChild
} // namespace Parent

//--- test.swift

import namespaces;

func test() {
  Parent.functionInInlineChild()
  Parent.functionInInlineChild2()
  Parent.InlineChild.functionInInlineChild()
  Parent.InlineChild.functionInInlineChild2()
  Parent.InlineChild.InlineChild2.functionInInlineChild2()
  // FIXME: fix the below case.
  Parent.Child.functionInInlineChildsNonInlineNamespace() // expected-error {{failed to produce diagnostic for expression}}
  Parent.InlineChild.Child.functionInInlineChildsNonInlineNamespace()
}
