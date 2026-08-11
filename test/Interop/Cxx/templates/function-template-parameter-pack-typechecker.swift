// RUN: %target-typecheck-verify-swift \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -I %S/Inputs
// RUN: %target-typecheck-verify-swift \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature ImportCxxMembersLazily \
// RUN:   -I %S/Inputs
//
// REQUIRES: swift_feature_ImportCxxMembersLazily

import FunctionTemplateParameterPack

// Function templates with a template parameter pack are not imported. Try to
// call them anyway to make sure we don't crash trying to resolve them.

public func callFreeFunctionTemplatesWithPack() {
  takesPack(1, Ts: CInt.self)
  // expected-error@-1{{cannot find 'takesPack' in scope}}
  takesTypeAndPack(1, 2, Ts: CInt.self)
  // expected-error@-1{{cannot find 'takesTypeAndPack' in scope}}
  unusedPack(Ts: CInt.self)
  // expected-error@-1{{cannot find 'unusedPack' in scope}}
  let _ = packInReturnTypeOnly(Ts: CInt.self)
  // expected-error@-1{{cannot find 'packInReturnTypeOnly' in scope}}
}

public func callMemberFunctionTemplatesWithPack(
    _ s: inout HasVariadicTemplateMembers) {
  s.memberTakesPack(1, Ts: CInt.self)
  // expected-error@-1{{value of type 'HasVariadicTemplateMembers' has no member 'memberTakesPack'}}
  HasVariadicTemplateMembers.staticMemberTakesPack(1, Ts: CInt.self)
  // expected-error@-1{{type 'HasVariadicTemplateMembers' has no member 'staticMemberTakesPack'}}
}
