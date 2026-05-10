// RUN: %target-swift-frontend -primary-file %s -swift-version 6 -enable-library-evolution -verify -c

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {}

struct NonSendable<T> {
  var x: T? = nil
}

public enum NonSendableEnum {
case case1
case case2
}

/////////////////
// MARK: Tests //
/////////////////

@MainActor struct DefaultArgumentInitializerTest<T> {
  var strategy: NonSendableEnum = .case1
  var strategy2 = NonSendable<T>()
  var strategy3 = NonSendableKlass()
}
