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

@inline(always) // expected-error{{'@inline(always)' cannot be combined with '@usableFromInline'; use '@inlinable' to allow the function body to be inlined into other modules}} {{+1:1-+1:18=@inlinable}}
@usableFromInline
func internalUFI() {}

// An internal '@inline(always)' helper called from a public '@inline(always)'
// function must be '@inlinable', not '@usableFromInline'.
@inline(always)
internal func hotHelper(_ x: Int) -> Int { x &+ 1 }
// expected-note@-1 2{{global function 'hotHelper' is not '@usableFromInline' or public}}
// expected-note@-2 2{{global function 'hotHelper' is marked '@inline(always)'; mark it '@inlinable' to make it usable from other '@inlinable' code}} {{-1:1-1=@inlinable }}

@inline(always)
public func exposedUsingHotHelper(_ x: Int) -> Int {
  hotHelper(x) // expected-error{{global function 'hotHelper' is internal and cannot be referenced from an '@inlinable' function}}
}

@inline(always)
@inlinable
internal func hotHelperInlinable(_ x: Int) -> Int { x &+ 1 }

@inline(always)
public func exposedUsingHotHelperInlinable(_ x: Int) -> Int {
  hotHelperInlinable(x) // okay
}

@inlinable
public func inlinableUsingHotHelper(_ x: Int) -> Int {
  hotHelper(x) // expected-error{{global function 'hotHelper' is internal and cannot be referenced from an '@inlinable' function}}
}

func internalFunc4() {} //expected-note{{global function 'internalFunc4()' is not '@usableFromInline' or public}}

@inline(always)
public var x: Int {
   get {
       internalFunc4() // expected-error{{global function 'internalFunc4()' is internal and cannot be referenced from an '@inlinable' function}}
       return 1
   }
}

class C {
  static func staticMethodInA() {} // okay

  @inline(always)
  final func myMethod() {} // okay

  @inline(always) // expected-error {{'@inline(always)' on class methods requires 'myMethod2()' to be marked 'final'}}
  func myMethod2() {}

  @inline(always)
  final var myVarFinal : Int { // okay
    return 0
  }

  @inline(always) // expected-error {{'@inline(always)' on class vars requires 'myVar' to be marked 'final'}}
  var myVar : Int {
    return 0
  }

  @inline(always)
  final var myVar2Final: Int { // okay
    get {
      return 1
    }
  }

  @inline(always) // expected-error {{'@inline(always)' on class vars requires 'myVar2' to be marked 'final'}}
  var myVar2: Int {
    get {
      return 1
    }
  }

  final var myVar3Final : Int { // okay
    @inline(always)
    get {
      return 1
    }
  }


  var myVar3: Int {
    @inline(always) // expected-error {{'@inline(always)' on class variable accessors whose variable declaration is not final are not allowed}}
    get {
      return 1
    }
  }

  @inline(always)
  static func myMethod3() {} // okay
}

extension C {
  @inline(always)
  final func myMethod10() {} // okay

  @inline(always)
  func myMethod11() {} // okay
}
