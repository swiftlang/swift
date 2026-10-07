// With the "interface" code generation model, clients emit their own copies
// of generic code, but refer to everything else by symbol. So, as with
// '@inlinable', code that clients emit can only refer to declarations that
// are public or '@usableFromInline'. That's an error when emitting a TBD file,
// which lists exactly those symbols, and a warning otherwise.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck %s -verify -verify-additional-prefix tbd- -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -emit-tbd-path %t/Lib.tbd -tbd-install_name Lib
// RUN: %target-swift-frontend -typecheck %s -verify -verify-additional-prefix warn- -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface

// Other code generation models copy internal declarations into clients too.
// RUN: %target-swift-frontend -typecheck %s -verify -parse-as-library -module-name Lib -enable-experimental-feature Embedded
// RUN: %target-swift-frontend -typecheck %s -verify -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=implementation

// REQUIRES: swift_feature_Embedded
// REQUIRES: VENDOR=apple

func internalHelper() -> Int { 1 }
// expected-tbd-note@-1 7{{global function 'internalHelper()' is not '@usableFromInline' or public}}
// expected-warn-note@-2 7{{global function 'internalHelper()' is not '@usableFromInline' or public}}

@usableFromInline func usableFromInlineHelper() -> Int { 2 }

private func privateHelper() -> Int { 3 }
// expected-tbd-note@-1 {{global function 'privateHelper()' is not '@usableFromInline' or public}}
// expected-warn-note@-2 {{global function 'privateHelper()' is not '@usableFromInline' or public}}

struct InternalType { init() {} }
// expected-tbd-note@-1 {{struct 'InternalType' is not '@usableFromInline' or public}}
// expected-warn-note@-2 {{struct 'InternalType' is not '@usableFromInline' or public}}
// expected-tbd-note@-3 {{initializer 'init()' is not '@usableFromInline' or public}}
// expected-warn-note@-4 {{initializer 'init()' is not '@usableFromInline' or public}}

func internalGeneric<T>(_ t: T) -> Int { internalHelper() }
// expected-tbd-note@-1 {{global function 'internalGeneric' is not '@usableFromInline' or public}}
// expected-warn-note@-2 {{global function 'internalGeneric' is not '@usableFromInline' or public}}

// Clients emit generic code.
public func publicGeneric<T>(_ t: T) -> Int {
  internalHelper()
  // expected-tbd-error@-1 {{global function 'internalHelper()' is internal and cannot be referenced from global function 'publicGeneric'}}
  // expected-warn-warning@-2 {{global function 'internalHelper()' is internal and cannot be referenced from global function 'publicGeneric'}}
  + usableFromInlineHelper()
  + privateHelper()
  // expected-tbd-error@-1 {{global function 'privateHelper()' is private and cannot be referenced from global function 'publicGeneric'}}
  // expected-warn-warning@-2 {{global function 'privateHelper()' is private and cannot be referenced from global function 'publicGeneric'}}
  + internalGeneric(t)
  // expected-tbd-error@-1 {{global function 'internalGeneric' is internal and cannot be referenced from global function 'publicGeneric'}}
  // expected-warn-warning@-2 {{global function 'internalGeneric' is internal and cannot be referenced from global function 'publicGeneric'}}
}

// So does a '@usableFromInline' generic function.
@usableFromInline func usableFromInlineGeneric<T>(_ t: T) -> Int {
  internalHelper()
  // expected-tbd-error@-1 {{global function 'internalHelper()' is internal and cannot be referenced from global function 'usableFromInlineGeneric'}}
  // expected-warn-warning@-2 {{global function 'internalHelper()' is internal and cannot be referenced from global function 'usableFromInlineGeneric'}}
}

// Including closures and types used within generic code.
public func genericWithClosure<T>(_ t: T) -> Int {
  let f = { internalHelper() }
  // expected-tbd-error@-1 {{global function 'internalHelper()' is internal and cannot be referenced from global function 'genericWithClosure'}}
  // expected-warn-warning@-2 {{global function 'internalHelper()' is internal and cannot be referenced from global function 'genericWithClosure'}}
  _ = InternalType()
  // expected-tbd-error@-1 {{struct 'InternalType' is internal and cannot be referenced from global function 'genericWithClosure'}}
  // expected-warn-warning@-2 {{struct 'InternalType' is internal and cannot be referenced from global function 'genericWithClosure'}}
  // expected-tbd-error@-3 {{initializer 'init()' is internal and cannot be referenced from global function 'genericWithClosure'}}
  // expected-warn-warning@-4 {{initializer 'init()' is internal and cannot be referenced from global function 'genericWithClosure'}}
  return f()
}

// And members of generic types.
public struct Box<T> {
  public var value: T
  public func helper() -> Int {
    internalHelper()
    // expected-tbd-error@-1 {{global function 'internalHelper()' is internal and cannot be referenced from instance method 'helper()'}}
    // expected-warn-warning@-2 {{global function 'internalHelper()' is internal and cannot be referenced from instance method 'helper()'}}
  }
}

// Members of a constrained extension that are concrete enough are emitted in
// this module, though.
extension Box where T == Int {
  public func concreteHelper() -> Int { internalHelper() }
}

// Clients don't emit non-generic code, or generic code they can't use.
public func publicNonGeneric() -> Int { internalHelper() + privateHelper() }

func internalGenericUsingPrivate<T>(_ t: T) -> Int { privateHelper() }

// A generic function in a protocol extension.
public protocol P {}
extension P {
  public func protocolExtensionMethod() -> Int {
    internalHelper()
    // expected-tbd-error@-1 {{global function 'internalHelper()' is internal and cannot be referenced from instance method 'protocolExtensionMethod()'}}
    // expected-warn-warning@-2 {{global function 'internalHelper()' is internal and cannot be referenced from instance method 'protocolExtensionMethod()'}}
  }
}

// Clients emit the initializers of global and static variables that don't
// have a unique definition.
@export(implementation)
public var implementationGlobal: Int = internalHelper()
// expected-tbd-error@-1 {{global function 'internalHelper()' is internal and cannot be referenced from var 'implementationGlobal'}}
// expected-warn-warning@-2 {{global function 'internalHelper()' is internal and cannot be referenced from var 'implementationGlobal'}}

public struct HasStatic {
  @export(implementation)
  public static var implementationStatic: Int = internalHelper()
  // expected-tbd-error@-1 {{global function 'internalHelper()' is internal and cannot be referenced from static property 'implementationStatic'}}
  // expected-warn-warning@-2 {{global function 'internalHelper()' is internal and cannot be referenced from static property 'implementationStatic'}}

  public static var interfaceStatic: Int = internalHelper()
}

public var interfaceGlobal: Int = internalHelper() + privateHelper()

// Clients access a struct's stored properties and form enum cases directly, so
// they don't need symbols.
@usableFromInline
struct StoredAndComputed {
  var stored: Int
  var computed: Int { stored }
  // expected-tbd-note@-1 {{property 'computed' is not '@usableFromInline' or public}}
  // expected-warn-note@-2 {{property 'computed' is not '@usableFromInline' or public}}
  // expected-warn-note@-3 {{getter for property 'computed' is not '@usableFromInline' or public}}

  @usableFromInline init() { stored = 0 }
}

@usableFromInline
enum Cases {
  case one
  case two(Int)
}

// A class's stored property can be dispatched through accessors.
@usableFromInline
final class StoredInClass {
  var stored: Int = 0
  // expected-tbd-note@-1 {{property 'stored' is not '@usableFromInline' or public}}
  // expected-warn-note@-2 {{property 'stored' is not '@usableFromInline' or public}}
  // expected-warn-note@-3 {{getter for property 'stored' is not '@usableFromInline' or public}}

  @usableFromInline init() {}
}

public func genericAccessingMembers<T>(_ t: T) -> Int {
  var s = StoredAndComputed()
  s.stored = MemoryLayout<T>.size
  let c = Cases.two(s.computed)
  // expected-tbd-error@-1 {{property 'computed' is internal and cannot be referenced from global function 'genericAccessingMembers'}}
  // expected-warn-warning@-2 {{property 'computed' is internal and cannot be referenced from global function 'genericAccessingMembers'}}
  // expected-warn-warning@-3 {{getter for property 'computed' is internal and cannot be referenced from global function 'genericAccessingMembers'}}
  if case .two(let value) = c { return value + StoredInClass().stored }
  // expected-tbd-error@-1 {{property 'stored' is internal and cannot be referenced from global function 'genericAccessingMembers'}}
  // expected-warn-warning@-2 {{property 'stored' is internal and cannot be referenced from global function 'genericAccessingMembers'}}
  // expected-warn-warning@-3 {{getter for property 'stored' is internal and cannot be referenced from global function 'genericAccessingMembers'}}
  return 0
}
