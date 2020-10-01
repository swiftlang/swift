// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-cxx-interop

import MemberVariables

var s = MyClass()
s.const_member = 42 // expected-error {{cannot assign to property: 'const_member' setter is inaccessible}}
