// RUN: %target-typecheck-verify-swift -package-name pkg -enable-experimental-feature InlineAlways

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

@usableFromInline
internal func internalFuncUFI() {}

@inline(always) // okay
@inlinable
func internalInlinable() {}

@inline(always) // okay
private func privateFunc() {}

@inline(always) // okay
fileprivate func filePrivateFunc() {}

@inline(always) // okay, implies @inlinable
public func publicFunc() {
  internalFuncUFI() // okay because useableFromInline
}

func internalFunc2() {} //expected-note{{global function 'internalFunc2()' is not '@usableFromInline' or public}}

@inline(always) // implies @inlinable
public func publicFuncUsingInternal() {
  internalFunc2() //expected-error{{global function 'internalFunc2()' is internal and cannot be referenced from an '@inlinable' function}}
}

func internalFunc3() {} //expected-note{{global function 'internalFunc3()' is not '@usableFromInline' or public}}

@inline(always) // implies @inlinable
package func packageFuncUsingInternal() {
  internalFunc3() //expected-error{{global function 'internalFunc3()' is internal and cannot be referenced from an '@inlinable' function}}
}

@inline(always)
func internalUsingInternal() {
  internalFunc2()
}

@inline(always) // okay
package func packageFunc() {}

@inline(always) // expected-error{{cannot use '@inline(always)' together with '@usableFromInline'}}
@usableFromInline
func internalUFI() {}

func internalFunc4() {} //expected-note{{global function 'internalFunc4()' is not '@usableFromInline' or public}}

@inline(always)
public var x: Int {
   get {
       internalFunc4() // expected-error{{global function 'internalFunc4()' is internal and cannot be referenced from an '@inlinable' function}}
       return 1
   }
}
