// RUN: %target-typecheck-verify-swift -swift-version 6 -cxx-interoperability-mode=default -I %S/Inputs %s

import FriendType

func requiresBitwiseCopyable<T: BitwiseCopyable>(_: T.Type) {}

func testBitwiseCopyable() {
  requiresBitwiseCopyable(NoFriend.self)
  requiresBitwiseCopyable(FriendClass.self)
  requiresBitwiseCopyable(FriendFunction.self)
  requiresBitwiseCopyable(FriendFunctionDefinition.self)
  requiresBitwiseCopyable(FriendTemplateSpecialization.self)
  requiresBitwiseCopyable(FriendWholeTemplate.self)
  requiresBitwiseCopyable(HasFriendlyMember.self)
}

func testFriendsAreNotMembers(_ value: FriendFunctionDefinition) {
  _ = getX(value)
  _ = value.getX() // expected-error {{value of type 'FriendFunctionDefinition' has no member 'getX'}}
}
