// RUN: %target-typecheck-verify-swift -I %S/Inputs -enable-cxx-interop

import MemberInline

var lhs = LoadableIntWrapper(value: 42)
let rhs = LoadableIntWrapper(value: 23)
let resultMinus = lhs - rhs

var lhsAddressOnly = AddressOnlyIntWrapper(42)
var rhsAddressOnly = AddressOnlyIntWrapper(23)
let resultMinusAddressOnly = lhsAddressOnly - rhsAddressOnly
