// RUN: %target-typecheck-verify-swift -package-name pkg -disable-availability-checking -enable-experimental-feature InlineAlways

// REQUIRES: swift_feature_InlineAlways

@inline(always) // okay
@inlinable
public func publicInlinableFunc() {}

@inline(always) // okay
@_alwaysEmitIntoClient
public func publicInlinableFunc2() {}

@inline(always) // okay
@inlinable
package func packageInlinableFunc() {}

@inline(always) // okay
internal func internalFunc() {}

@inline(always) // okay
@inlinable
func internalInlinable() {}

@inline(always) // okay
private func privateFunc() {}

@inline(always) // okay
fileprivate func filePrivateFunc() {}

@inline(always) //  // expected-error{{'@inline(always)' on public or package declarations must be used together with '@inlinable'}}
public func publicFunc() {}

@inline(always) //  // expected-error{{'@inline(always)' on public or package declarations must be used together with '@inlinable'}}
package func packageFunc() {}

@inline(always) // expected-error{{cannot use '@inline(always)' together with '@usableFromInline'}}
@usableFromInline
func internalUFI() {}
