// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-experimental-cxx-interop -verify-additional-file %S/Inputs/member-variables.h

import MemberVariables

var s1 = MyClass()
s1.const_member = 42 // expected-error {{cannot assign to property: 'const_member' setter is inaccessible}}

// TODO: rdar://148437848 Swift doesn't support fields marked with [[no_unique_address]] 
var s2 = HasNoUniqueAddressField()
_ = s2.simpleField.const_member
_ = s2.noUniqueAddressField.const_member  // expected-error {{value of type 'HasNoUniqueAddressField' has no member 'noUniqueAddressField'}}
