// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-cxx-interop

import CxxMemberVariables

var s = MyClass()
s.const_member = 42 // expected-error {{cannot assign to property: 'const_member' setter is inaccessible}}
