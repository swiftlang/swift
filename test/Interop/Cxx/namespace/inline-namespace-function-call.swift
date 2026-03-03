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
int GlobalVariableInInlineChild2;
struct GlobalTypeInInlineChild2 {};
} // namespace InlineChild2
void functionInInlineChild();
int GlobalVariableInInlineChild;
struct GlobalTypeInInlineChild {};
namespace Child {
void functionInInlineChildsNonInlineNamespace();
} // namespace Child
} // namespace InlineChild
} // namespace Parent

//--- test.swift

import namespaces;

func test() {
  Parent.functionInInlineChild()
  Parent.GlobalVariableInInlineChild = 1
  Parent.functionInInlineChild2()
  Parent.GlobalVariableInInlineChild2 = 2
  Parent.InlineChild.functionInInlineChild()
  Parent.InlineChild.GlobalVariableInInlineChild = 3
  Parent.InlineChild.functionInInlineChild2()
  Parent.InlineChild.GlobalVariableInInlineChild2 = 4
  Parent.InlineChild.InlineChild2.functionInInlineChild2()
  Parent.InlineChild.InlineChild2.GlobalVariableInInlineChild2 = 5
  // FIXME: fix the below case.
  Parent.Child.functionInInlineChildsNonInlineNamespace() // expected-error {{failed to produce diagnostic for expression}}
  Parent.InlineChild.Child.functionInInlineChildsNonInlineNamespace()
}

func testTypes(x: Parent.GlobalTypeInInlineChild, y: Parent.GlobalTypeInInlineChild2,
               x2: Parent.InlineChild.GlobalTypeInInlineChild,
               y2: Parent.InlineChild.GlobalTypeInInlineChild2,
               y3: Parent.InlineChild.InlineChild2.GlobalTypeInInlineChild2) {}
