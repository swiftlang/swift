// RUN: %empty-directory(%t)

// RUN: mkdir -p %t/SDK/Frameworks/SomeModule.framework/Modules/SomeModule.swiftmodule
// RUN: %target-swift-frontend \
// RUN:     -emit-module \
// RUN:     -module-name SomeModule \
// RUN:     -o %t/SDK/Frameworks/SomeModule.framework/Modules/SomeModule.swiftmodule/%module-target-triple.swiftmodule \
// RUN:     -swift-version 5 \
// RUN:     %s

// RUN: %sourcekitd-test -req=interface-gen -module SomeModule -- -sdk %t/SDK -Fsystem %t/SDK/Frameworks -target %target-triple > %t.response
// RUN: %diff -u %s.response %t.response

public struct SomeValue {
  internal var internalValue: Int { return 1 }
  public var _secretValue: Int { return 1 }
  public var publicValue: Int { return 1 }

  internal func internalMethod() -> Int { return 1 }
  public func _secretMethod() -> Int { return 1 }
  public func publicMethod() -> Int { return 1 }

  internal init(internal: Int) {}
  public init(_secret: Int) {}
  public init(public: Int) {}
}

internal func internalFunc() {}
public func _secretFunc() {}
public func publicFunc() {}

internal class InternalClass {}
public class _SecretClass {
  public var publicVarInSeretClass: Int = 0
}
public class PublicClass {}
