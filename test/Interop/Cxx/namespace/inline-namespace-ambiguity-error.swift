// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unknown -I %t/Inputs  %t/test.swift  -enable-experimental-cxx-interop

//--- Inputs/module.modulemap
module namespaces {
  header "test.h"
  requires cplusplus
}
//--- Inputs/test.h
namespace Parent {
inline namespace InlineChild {

struct StructInInlineChildAndParent {};

struct StructInInlineChildAndSiblingInlineChild {};

inline namespace SecondInlineChild {
} // namespace SecondInlineChild
} // namespace InlineChild

struct StructInInlineChildAndParent {};

inline namespace SiblingInlineChild {

struct StructInInlineChildAndSiblingInlineChild {};

} // namespace SiblingInlineChild

} // namespace Parent

//--- test.swift

import namespaces;

extension Parent.StructInInlineChildAndParent { // expected-error {{ambiguous type name 'StructInInlineChildAndParent' in 'Parent'}}
}

extension Parent.StructInInlineChildAndSiblingInlineChild { // expected-error {{ambiguous type name 'StructInInlineChildAndSiblingInlineChild' in 'Parent'}}
}

extension Parent.InlineChild.StructInInlineChildAndParent { // ok
  var string: String {
    return ""
  }
}

extension Parent.InlineChild.StructInInlineChildAndSiblingInlineChild { // ok
  var string: String {
    return ""
  }
}

extension Parent.InlineChild.SecondInlineChild.StructInInlineChildAndParent { // expected-error {{'StructInInlineChildAndParent' is not a member type of enum '__ObjC.Parent.InlineChild.SecondInlineChild'}}
}
