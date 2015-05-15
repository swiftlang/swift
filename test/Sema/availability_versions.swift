// RUN: %target-parse-verify-swift
// RUN: not %target-swift-frontend -parse %s 2>&1 | FileCheck %s '--implicit-check-not=<unknown>:0'

// Make sure we do not emit availability errors or warnings when -disable-availability-checking is passed
// RUN: not %target-swift-frontend -parse -disable-availability-checking %s 2>&1 | FileCheck %s '--implicit-check-not=error:' '--implicit-check-not=warning:'

// REQUIRES: OS=macosx

func markUsed<T>(t: T) {}

@available(OSX, introduced=10.9)
func globalFuncAvailableOn10_9() -> Int { return 9 }

@available(OSX, introduced=10.10)
func globalFuncAvailableOn10_10() -> Int { return 10 }

@available(OSX, introduced=10.11)
func globalFuncAvailableOn10_11() -> Int { return 11 }

// Top level should reflect the minimum deployment target.
let ignored1: Int = globalFuncAvailableOn10_9()

let ignored2: Int = globalFuncAvailableOn10_10() // expected-error {{'globalFuncAvailableOn10_10()' is only available on OS X 10.10 or newer}}
    // expected-note@-1 {{guard with version check}}

let ignored3: Int = globalFuncAvailableOn10_11() // expected-error {{'globalFuncAvailableOn10_11()' is only available on OS X 10.11 or newer}}
    // expected-note@-1 {{guard with version check}}

// Functions without annotations should reflect the minimum deployment target.
func functionWithoutAvailability() {
  let _: Int = globalFuncAvailableOn10_9()

  let _: Int = globalFuncAvailableOn10_10() // expected-error {{'globalFuncAvailableOn10_10()' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}

  let _: Int = globalFuncAvailableOn10_11() // expected-error {{'globalFuncAvailableOn10_11()' is only available on OS X 10.11 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}
}

// Functions with annotations should refine their bodies.
@available(OSX, introduced=10.10)
func functionAvailableOn10_10() {
  let _: Int = globalFuncAvailableOn10_9()
  let _: Int = globalFuncAvailableOn10_10()

  // Nested functions should get their own refinement context.
  @available(OSX, introduced=10.11)
  func innerFunctionAvailableOn10_11() {
    let _: Int = globalFuncAvailableOn10_9()
    let _: Int = globalFuncAvailableOn10_10()
    let _: Int = globalFuncAvailableOn10_11()
  }

  let _: Int = globalFuncAvailableOn10_11() // expected-error {{'globalFuncAvailableOn10_11()' is only available on OS X 10.11 or newer}}
      // expected-note@-1 {{guard with version check}}
}

// Don't allow script-mode globals to marked potentially unavailable. Their
// initializers are eagerly executed.
@available(OSX, introduced=10.10) // expected-error {{global variable cannot be marked potentially unavailable with 'introduced=' in script mode}}
var potentiallyUnavailableGlobalInScriptMode: Int = globalFuncAvailableOn10_10()

// Still allow other availability annotations on script-mode globals
@available(OSX, deprecated=10.10)
var deprecatedGlobalInScriptMode: Int = 5

if #available(OSX 10.10, *) {
  let _: Int = globalFuncAvailableOn10_10()
  let _: Int = globalFuncAvailableOn10_11() // expected-error {{'globalFuncAvailableOn10_11()' is only available on OS X 10.11 or newer}}
      // expected-note@-1 {{guard with version check}}
}

if #available(OSX 10.10, *) {
  let _: Int = globalFuncAvailableOn10_10()
  let _: Int = globalFuncAvailableOn10_11() // expected-error {{'globalFuncAvailableOn10_11()' is only available on OS X 10.11 or newer}}
      // expected-note@-1 {{guard with version check}}
} else {
  let _: Int = globalFuncAvailableOn10_9()
  let _: Int = globalFuncAvailableOn10_10() // expected-error {{'globalFuncAvailableOn10_10()' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{guard with version check}}
}

@available(OSX, introduced=10.10)
@available(iOS, introduced=8.0)
func globalFuncAvailableOnOSX10_10AndiOS8_0() -> Int { return 10 }

if #available(OSX 10.10, iOS 8.0, *) {
  let _: Int = globalFuncAvailableOnOSX10_10AndiOS8_0()
}

if #available(OSX 10.10, OSX 10.11, *) {  // expected-error {{conditions for 'OSX' already specified for this query}}
}

if #available(iOS 9.0) {  // expected-error {{condition required for target platform 'OSX'}} expected-error {{check must handle potential future platforms with '*'}} {{22-22=, *}}
  let _: Int = globalFuncAvailableOnOSX10_10AndiOS8_0() // expected-error {{'globalFuncAvailableOnOSX10_10AndiOS8_0()' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{guard with version check}}
}

if #available(OSX 10.10, iOS 9.0) {  // expected-error {{check must handle potential future platforms with '*'}} {{33-33=, *}}
}
// Multiple unavailable references in a single statement

let ignored4: (Int, Int) = (globalFuncAvailableOn10_10(), globalFuncAvailableOn10_11()) // expected-error {{'globalFuncAvailableOn10_10()' is only available on OS X 10.10 or newer}}  expected-error {{'globalFuncAvailableOn10_11()' is only available on OS X 10.11 or newer}}
    // expected-note@-1 2{{guard with version check}}


globalFuncAvailableOn10_9()

let ignored5 = globalFuncAvailableOn10_10 // expected-error {{'globalFuncAvailableOn10_10()' is only available on OS X 10.10 or newer}}
    // expected-note@-1 {{guard with version check}}

globalFuncAvailableOn10_10() // expected-error {{'globalFuncAvailableOn10_10()' is only available on OS X 10.10 or newer}}
    // expected-note@-1 {{guard with version check}}

// Overloaded global functions
@available(OSX, introduced=10.9)
func overloadedFunction() {}

@available(OSX, introduced=10.10)
func overloadedFunction(on1010: Int) {}

overloadedFunction()
overloadedFunction(0) // expected-error {{'overloadedFunction' is only available on OS X 10.10 or newer}}
    // expected-note@-1 {{guard with version check}}

// Unavailable methods

class ClassWithUnavailableMethod {
  @available(OSX, introduced=10.9)
  func methAvailableOn10_9() {}
  
  @available(OSX, introduced=10.10)
  func methAvailableOn10_10() {}
  
  @available(OSX, introduced=10.10)
  class func classMethAvailableOn10_10() {}
  
  func someOtherMethod() {
    methAvailableOn10_9()
    methAvailableOn10_10() // expected-error {{'methAvailableOn10_10()' is only available on OS X 10.10 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing class}}
        // expected-note@-2 {{add @available attribute to enclosing instance method}}
        // expected-note@-3 {{guard with version check}}
  }
}

func callUnavailableMethods(o: ClassWithUnavailableMethod) {
  let m10_9 = o.methAvailableOn10_9
  m10_9()
  
  let m10_10 = o.methAvailableOn10_10 // expected-error {{'methAvailableOn10_10()' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}

  m10_10()
  
  o.methAvailableOn10_9()
  o.methAvailableOn10_10() // expected-error {{'methAvailableOn10_10()' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}
}

func callUnavailableMethodsViaIUO(o: ClassWithUnavailableMethod!) {
  let m10_9 = o.methAvailableOn10_9
  m10_9()
  
  let m10_10 = o.methAvailableOn10_10 // expected-error {{'methAvailableOn10_10()' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}

  m10_10()
  
  o.methAvailableOn10_9()
  o.methAvailableOn10_10() // expected-error {{'methAvailableOn10_10()' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}
}

func callUnavailableClassMethod() {
  ClassWithUnavailableMethod.classMethAvailableOn10_10() // expected-error {{'classMethAvailableOn10_10()' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}
  
  let m10_10 = ClassWithUnavailableMethod.classMethAvailableOn10_10 // expected-error {{'classMethAvailableOn10_10()' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}

  m10_10()
}

class SubClassWithUnavailableMethod : ClassWithUnavailableMethod {
  func someMethod() {
    methAvailableOn10_9()
    methAvailableOn10_10() // expected-error {{'methAvailableOn10_10()' is only available on OS X 10.10 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing class}}
        // expected-note@-2 {{add @available attribute to enclosing instance method}}
        // expected-note@-3 {{guard with version check}}
  }
}

class SubClassOverridingUnavailableMethod : ClassWithUnavailableMethod {

  override func methAvailableOn10_10() {
    methAvailableOn10_9()
    super.methAvailableOn10_10() // expected-error {{'methAvailableOn10_10()' is only available on OS X 10.10 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing class}}
        // expected-note@-2 {{add @available attribute to enclosing instance method}}
        // expected-note@-3 {{guard with version check}}
    
    let m10_9 = super.methAvailableOn10_9
    m10_9()
    
    let m10_10 = super.methAvailableOn10_10 // expected-error {{'methAvailableOn10_10()' is only available on OS X 10.10 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing class}}
        // expected-note@-2 {{add @available attribute to enclosing instance method}}
        // expected-note@-3 {{guard with version check}}
    m10_10()
  }
  
  func someMethod() {
    methAvailableOn10_9()
    // Calling our override should be fine
    methAvailableOn10_10()
  }
}

class ClassWithUnavailableOverloadedMethod {
  @available(OSX, introduced=10.9)
  func overloadedMethod() {}

  @available(OSX, introduced=10.10)
  func overloadedMethod(on1010: Int) {}
}

func callUnavailableOverloadedMethod(o: ClassWithUnavailableOverloadedMethod) {
  o.overloadedMethod()
  o.overloadedMethod(0) // expected-error {{'overloadedMethod' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}
}

// Initializers

class ClassWithUnavailableInitializer {
  @available(OSX, introduced=10.9)
  required init() {  }
  
  @available(OSX, introduced=10.10)
  required init(_ val: Int) {  }
  
  convenience init(s: String) {
    self.init(5) // expected-error {{'init' is only available on OS X 10.10 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing class}}
        // expected-note@-2 {{add @available attribute to enclosing initializer}}
        // expected-note@-3 {{guard with version check}}
  }
  
  @available(OSX, introduced=10.10)
  convenience init(onlyOn1010: String) {
    self.init(5)
  }
}

func callUnavailableInitializer() {
  ClassWithUnavailableInitializer()
  ClassWithUnavailableInitializer(5) // expected-error {{'init' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}
  
  let i = ClassWithUnavailableInitializer.self 
  i()
  i(5) // expected-error {{'init' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}
}

class SuperWithWithUnavailableInitializer {
  @available(OSX, introduced=10.9)
  init() {  }
  
  @available(OSX, introduced=10.10)
  init(_ val: Int) {  }
}

class SubOfClassWithUnavailableInitializer : SuperWithWithUnavailableInitializer {
  override init(_ val: Int) {
    super.init(5) // expected-error {{'init' is only available on OS X 10.10 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing class}}
        // expected-note@-2 {{add @available attribute to enclosing initializer}}
        // expected-note@-3 {{guard with version check}}
  }
  
  override init() {
    super.init()
  }
  
  @available(OSX, introduced=10.10)
  init(on1010: Int) {
    super.init(22)
  }
}

// Properties

class ClassWithUnavailableProperties {

  @available(OSX, introduced=10.9) // expected-error {{stored properties cannot be marked potentially unavailable with 'introduced='}}
  var nonLazyAvailableOn10_9Stored: Int = 9

  @available(OSX, introduced=10.10) // expected-error {{stored properties cannot be marked potentially unavailable with 'introduced='}}
  var nonLazyAvailableOn10_10Stored : Int = 10

  @available(OSX, introduced=10.10) // expected-error {{stored properties cannot be marked potentially unavailable with 'introduced='}}
  let nonLazyLetAvailableOn10_10Stored : Int = 10

  // Make sure that we don't emit a Fix-It to mark a stored property as potentially unavailable.
  // We don't support potentially unavailable stored properties yet.
  var storedPropertyOfUnavailableType: ClassAvailableOn10_10? = nil // expected-error {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing class}}

  @available(OSX, introduced=10.9)
  lazy var availableOn10_9Stored: Int = 9
  
  @available(OSX, introduced=10.10)
  lazy var availableOn10_10Stored : Int = 10

  @available(OSX, introduced=10.9)
  var availableOn10_9Computed: Int {
    get {
      let _: Int = availableOn10_10Stored // expected-error {{'availableOn10_10Stored' is only available on OS X 10.10 or newer}}
          // expected-note@-1 {{add @available attribute to enclosing class}}
          // expected-note@-2 {{guard with version check}}
      
      if #available(OSX 10.10, *) {
        let _: Int = availableOn10_10Stored
      }
      
      return availableOn10_9Stored
    }
    set(newVal) {
      availableOn10_9Stored = newVal
    }
  }
  
  @available(OSX, introduced=10.10)
  var availableOn10_10Computed: Int {
    get {
      return availableOn10_10Stored
    }
    set(newVal) {
      availableOn10_10Stored = newVal
    }
  }
  
  var propWithSetterOnlyAvailableOn10_10 : Int {
    get {
      globalFuncAvailableOn10_10() // expected-error {{'globalFuncAvailableOn10_10()' is only available on OS X 10.10 or newer}}
          // expected-note@-1 {{add @available attribute to enclosing class}}
          // expected-note@-2 {{add @available attribute to enclosing var}}
          // expected-note@-3 {{guard with version check}}
      return 0
    }
    @available(OSX, introduced=10.10)
    set(newVal) {
    globalFuncAvailableOn10_10()
    }
  }
  
  var propWithGetterOnlyAvailableOn10_10 : Int {
    @available(OSX, introduced=10.10)
    get {
      globalFuncAvailableOn10_10()
      return 0
    }
    set(newVal) {
      globalFuncAvailableOn10_10() // expected-error {{'globalFuncAvailableOn10_10()' is only available on OS X 10.10 or newer}}
          // expected-note@-1 {{add @available attribute to enclosing class}}
          // expected-note@-2 {{add @available attribute to enclosing var}}
          // expected-note@-3 {{guard with version check}}
    }
  }
  
  var propWithGetterAndSetterOnlyAvailableOn10_10 : Int {
    @available(OSX, introduced=10.10)
    get {
      return 0
    }
    @available(OSX, introduced=10.10)
    set(newVal) {
    }
  }
  
  var propWithSetterOnlyAvailableOn10_10ForNestedMemberRef : ClassWithUnavailableProperties {
    get {
      return ClassWithUnavailableProperties()
    }
    @available(OSX, introduced=10.10)
    set(newVal) {
    }
  }
  
  var propWithGetterOnlyAvailableOn10_10ForNestedMemberRef : ClassWithUnavailableProperties {
    @available(OSX, introduced=10.10)
    get {
      return ClassWithUnavailableProperties()
    }
    set(newVal) {
    }
  }
}

@available(OSX, introduced=10.10)
class ClassWithReferencesInInitializers {
  var propWithInitializer10_10: Int = globalFuncAvailableOn10_10()

  var propWithInitializer10_11: Int = globalFuncAvailableOn10_11() // expected-error {{'globalFuncAvailableOn10_11()' is only available on OS X 10.11 or newer}}

  lazy var lazyPropWithInitializer10_10: Int = globalFuncAvailableOn10_10()

  lazy var lazyPropWithInitializer10_11: Int = globalFuncAvailableOn10_11() // expected-error {{'globalFuncAvailableOn10_11()' is only available on OS X 10.11 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing var}}
}

func accessUnavailableProperties(o: ClassWithUnavailableProperties) {
  // Stored properties
  let _: Int = o.availableOn10_9Stored
  let _: Int = o.availableOn10_10Stored // expected-error {{'availableOn10_10Stored' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}
  
  o.availableOn10_9Stored = 9
  o.availableOn10_10Stored = 10 // expected-error {{'availableOn10_10Stored' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}

  // Computed Properties
  let _: Int = o.availableOn10_9Computed
  let _: Int = o.availableOn10_10Computed // expected-error {{'availableOn10_10Computed' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}
  
  o.availableOn10_9Computed = 9
  o.availableOn10_10Computed = 10 // expected-error {{'availableOn10_10Computed' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}
  
  // Getter allowed on 10.9 but setter is not
  let _: Int = o.propWithSetterOnlyAvailableOn10_10
  o.propWithSetterOnlyAvailableOn10_10 = 5 // expected-error {{setter for 'propWithSetterOnlyAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}
  
  if #available(OSX 10.10, *) {
    // Setter is allowed on 10.10 and greater
    o.propWithSetterOnlyAvailableOn10_10 = 5
  }
  
  // Setter allowed on 10.9 but getter is not
  o.propWithGetterOnlyAvailableOn10_10 = 5
  let _: Int = o.propWithGetterOnlyAvailableOn10_10 // expected-error {{getter for 'propWithGetterOnlyAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}

  if #available(OSX 10.10, *) {
    // Getter is allowed on 10.10 and greater
    let _: Int = o.propWithGetterOnlyAvailableOn10_10
  }
  
  // Tests for nested member refs
  
  // Both getters are potentially unavailable.
  let _: Int = o.propWithGetterOnlyAvailableOn10_10ForNestedMemberRef.propWithGetterOnlyAvailableOn10_10 // expected-error {{getter for 'propWithGetterOnlyAvailableOn10_10ForNestedMemberRef' is only available on OS X 10.10 or newer}} expected-error {{getter for 'propWithGetterOnlyAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 2{{add @available attribute to enclosing global function}}
      // expected-note@-2 2{{guard with version check}}

  // Nested getter is potentially unavailable, outer getter is available
  let _: Int = o.propWithGetterOnlyAvailableOn10_10ForNestedMemberRef.propWithSetterOnlyAvailableOn10_10 // expected-error {{getter for 'propWithGetterOnlyAvailableOn10_10ForNestedMemberRef' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}

  // Nested getter is available, outer getter is potentially unavailable
  let _:Int = o.propWithSetterOnlyAvailableOn10_10ForNestedMemberRef.propWithGetterOnlyAvailableOn10_10 // expected-error {{getter for 'propWithGetterOnlyAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}

  // Both getters are always available.
  let _: Int = o.propWithSetterOnlyAvailableOn10_10ForNestedMemberRef.propWithSetterOnlyAvailableOn10_10
  
  
  // Nesting in source of assignment
  var v: Int
  
  v = o.propWithGetterOnlyAvailableOn10_10 // expected-error {{getter for 'propWithGetterOnlyAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}

  v = (o.propWithGetterOnlyAvailableOn10_10) // expected-error {{getter for 'propWithGetterOnlyAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}
  
  // Inout requires access to both getter and setter
  
  func takesInout(inout i : Int) { }
  
  takesInout(&o.propWithGetterOnlyAvailableOn10_10) // expected-error {{cannot pass as inout because getter for 'propWithGetterOnlyAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}

  takesInout(&o.propWithSetterOnlyAvailableOn10_10) // expected-error {{cannot pass as inout because setter for 'propWithSetterOnlyAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}
  
  takesInout(&o.propWithGetterAndSetterOnlyAvailableOn10_10) // expected-error {{cannot pass as inout because getter for 'propWithGetterAndSetterOnlyAvailableOn10_10' is only available on OS X 10.10 or newer}} expected-error {{cannot pass as inout because setter for 'propWithGetterAndSetterOnlyAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 2{{add @available attribute to enclosing global function}}
      // expected-note@-2 2{{guard with version check}}

  takesInout(&o.availableOn10_9Computed)
  takesInout(&o.propWithGetterOnlyAvailableOn10_10ForNestedMemberRef.availableOn10_9Computed) // expected-error {{getter for 'propWithGetterOnlyAvailableOn10_10ForNestedMemberRef' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}
}

// Enums

@available(OSX, introduced=10.10)
enum EnumIntroducedOn10_10 {
 case Element
}

@available(OSX, introduced=10.11)
enum EnumIntroducedOn10_11 {
 case Element
}

@available(OSX, introduced=10.10)
enum CompassPoint {
  case North
  case South
  case East

  @available(OSX, introduced=10.11)
  case West

  case WithAvailableByEnumPayload(p : EnumIntroducedOn10_10)

  @available(OSX, introduced=10.11)
  case WithAvailableByEnumElementPayload(p : EnumIntroducedOn10_11)

  @available(OSX, introduced=10.11)
  case WithAvailableByEnumElementPayload1(p : EnumIntroducedOn10_11), WithAvailableByEnumElementPayload2(p : EnumIntroducedOn10_11)

  case WithUnavailablePayload(p : EnumIntroducedOn10_11) // expected-error {{'EnumIntroducedOn10_11' is only available on OS X 10.11 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing case}}

    case WithUnavailablePayload1(p : EnumIntroducedOn10_11), WithUnavailablePayload2(p : EnumIntroducedOn10_11) // expected-error 2{{'EnumIntroducedOn10_11' is only available on OS X 10.11 or newer}}
      // expected-note@-1 2{{add @available attribute to enclosing case}}
}

@available(OSX, introduced=10.11)
func functionTakingEnumIntroducedOn10_11(e: EnumIntroducedOn10_11) { }

func useEnums() {
  let _: CompassPoint = .North // expected-error {{'CompassPoint' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}

  if #available(OSX 10.10, *) {
    let _: CompassPoint = .North

    let _: CompassPoint = .West // expected-error {{'West' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing global function}}
        // expected-note@-2 {{guard with version check}}

  }

  if #available(OSX 10.11, *) {
    let _: CompassPoint = .West
  }

  // Pattern matching on an enum element does not require it to be definitely available
  if #available(OSX 10.10, *) {
    let point: CompassPoint = .North
    switch (point) {
      case .North, .South, .East:
        markUsed("NSE")
      case .West: // We do not expect an error here
        markUsed("W")

      case .WithAvailableByEnumElementPayload(let p):
        markUsed("WithAvailableByEnumElementPayload")

        // For the moment, we do not incorporate enum element availability into 
        // TRC construction. Perhaps we should?
        functionTakingEnumIntroducedOn10_11(p)  // expected-error {{'functionTakingEnumIntroducedOn10_11' is only available on OS X 10.11 or newer}}
          // expected-note@-1 {{add @available attribute to enclosing global function}}
          // expected-note@-2 {{guard with version check}}
    }
  }
}

// Classes

@available(OSX, introduced=10.9)
class ClassAvailableOn10_9 {
  func someMethod() {}
  class func someClassMethod() {}
  var someProp : Int = 22
}

@available(OSX, introduced=10.10)
class ClassAvailableOn10_10 { // expected-note {{enclosing scope here}}
  func someMethod() {}
  class func someClassMethod() {
    let _ = ClassAvailableOn10_10()
  }
  var someProp : Int = 22

  @available(OSX, introduced=10.9) // expected-error {{declaration cannot be more available than enclosing scope}}
  func someMethodAvailableOn10_9() { }

  @available(OSX, introduced=10.11)
  var propWithGetter: Int { // expected-note{{enclosing scope here}}
    @available(OSX, introduced=10.10) // expected-error {{declaration cannot be more available than enclosing scope}}
    get { return 0 }
  }
}

func classAvailability() {
  ClassAvailableOn10_9.someClassMethod()
  ClassAvailableOn10_10.someClassMethod() // expected-error {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}

  ClassAvailableOn10_9.self
  ClassAvailableOn10_10.self // expected-error {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}
  
  let o10_9 = ClassAvailableOn10_9()
  let o10_10 = ClassAvailableOn10_10() // expected-error {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}
  
  o10_9.someMethod()
  o10_10.someMethod()
  
  let _ = o10_9.someProp
  let _ = o10_10.someProp 
}

func castingUnavailableClass(o : AnyObject) {
  let _ = o as! ClassAvailableOn10_10 // expected-error {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}

  let _ = o as? ClassAvailableOn10_10 // expected-error {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}

  let _ = o is ClassAvailableOn10_10 // expected-error {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}
}

protocol Createable {
  init()
}

@available(OSX, introduced=10.10)
class ClassAvailableOn10_10_Createable : Createable { 
  required init() {}
}

func create<T : Createable>() -> T {
  return T()
}

class ClassWithGenericTypeParameter<T> { }

class ClassWithTwoGenericTypeParameter<T, S> { }

func classViaTypeParameter() {
  let _ : ClassAvailableOn10_10_Createable = // expected-error {{'ClassAvailableOn10_10_Createable' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}
      create()
      
  let _ = create() as
      ClassAvailableOn10_10_Createable // expected-error {{'ClassAvailableOn10_10_Createable' is only available on OS X 10.10 or newer}}
          // expected-note@-1 {{add @available attribute to enclosing global function}}
          // expected-note@-2 {{guard with version check}}

  let _ = [ClassAvailableOn10_10]() // expected-error {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}

  let _: ClassWithGenericTypeParameter<ClassAvailableOn10_10> = ClassWithGenericTypeParameter() // expected-error {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}

  let _: ClassWithTwoGenericTypeParameter<ClassAvailableOn10_10, String> = ClassWithTwoGenericTypeParameter() // expected-error {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}

  let _: ClassWithTwoGenericTypeParameter<String, ClassAvailableOn10_10> = ClassWithTwoGenericTypeParameter() // expected-error {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}

  let _: ClassWithTwoGenericTypeParameter<ClassAvailableOn10_10, ClassAvailableOn10_10> = ClassWithTwoGenericTypeParameter() // expected-error 2{{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 2{{add @available attribute to enclosing global function}}
      // expected-note@-2 2{{guard with version check}}

  let _: ClassAvailableOn10_10? = nil // expected-error {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}

}

// Unavailable class used in declarations

class ClassWithDeclarationsOfUnavailableClasses {

  @available(OSX, introduced=10.10)
  init() {
    unavailablePropertyOfUnavailableType = ClassAvailableOn10_10()
    unavailablePropertyOfUnavailableType = ClassAvailableOn10_10()
  }

  var propertyOfUnavailableType: ClassAvailableOn10_10 // expected-error {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing class}}

  @available(OSX, introduced=10.10)
  lazy var unavailablePropertyOfUnavailableType: ClassAvailableOn10_10 = ClassAvailableOn10_10()
  
  @available(OSX, introduced=10.10)
  lazy var unavailablePropertyOfOptionalUnavailableType: ClassAvailableOn10_10? = nil

  @available(OSX, introduced=10.10)
  lazy var unavailablePropertyOfUnavailableTypeWithInitializer: ClassAvailableOn10_10 = ClassAvailableOn10_10()
  
  @available(OSX, introduced=10.10)
  static var unavailableStaticPropertyOfUnavailableType: ClassAvailableOn10_10 = ClassAvailableOn10_10()

  @available(OSX, introduced=10.10)
  static var unavailableStaticPropertyOfOptionalUnavailableType: ClassAvailableOn10_10?

  func methodWithUnavailableParameterType(o : ClassAvailableOn10_10) { // expected-error {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing class}}
      // expected-note@-2 {{add @available attribute to enclosing instance method}}

  }
  
  @available(OSX, introduced=10.10)
  func unavailableMethodWithUnavailableParameterType(o : ClassAvailableOn10_10) {
  }
  
  func methodWithUnavailableReturnType() -> ClassAvailableOn10_10  { // expected-error {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing class}}
      // expected-note@-2 {{add @available attribute to enclosing instance method}}

    return ClassAvailableOn10_10() // expected-error {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing class}}
      // expected-note@-2 {{add @available attribute to enclosing instance method}}
      // expected-note@-3 {{guard with version check}}
  }
  
  @available(OSX, introduced=10.10)
  func unavailableMethodWithUnavailableReturnType() -> ClassAvailableOn10_10  {
    return ClassAvailableOn10_10()
  }

  func methodWithUnavailableLocalDeclaration() {
    let _ : ClassAvailableOn10_10 = methodWithUnavailableReturnType() // expected-error {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing class}}
      // expected-note@-2 {{add @available attribute to enclosing instance method}}
      // expected-note@-3 {{guard with version check}}
  }
  
  @available(OSX, introduced=10.10)
  func unavailableMethodWithUnavailableLocalDeclaration() {
    let _ : ClassAvailableOn10_10 = methodWithUnavailableReturnType()
  }
}

func referToUnavailableStaticProperty() {
  let _ = ClassWithDeclarationsOfUnavailableClasses.unavailableStaticPropertyOfUnavailableType // expected-error {{'unavailableStaticPropertyOfUnavailableType' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}
}

class ClassExtendingUnavailableClass : ClassAvailableOn10_10 { // expected-error {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
    // expected-note@-1 {{add @available attribute to enclosing class}}

}

@available(OSX, introduced=10.10)
class UnavailableClassExtendingUnavailableClass : ClassAvailableOn10_10 {
}

// Method availability is contravariant

class SuperWithAlwaysAvailableMembers {
  func shouldAlwaysBeAvailableMethod() { // expected-note {{overridden declaration is here}}
  }
  
  var shouldAlwaysBeAvailableProperty: Int { // expected-note {{overridden declaration is here}}
    get { return 9 }
    set(newVal) {}
  }

  var setterShouldAlwaysBeAvailableProperty: Int {
    get { return 9 }
    set(newVal) {} // expected-note {{overridden declaration is here}}
  }

  var getterShouldAlwaysBeAvailableProperty: Int {
    get { return 9 } // expected-note {{overridden declaration is here}}
    set(newVal) {}
  }
}

class SubWithLimitedMemberAvailability : SuperWithAlwaysAvailableMembers {
  @available(OSX, introduced=10.10)
  override func shouldAlwaysBeAvailableMethod() { // expected-error {{overriding 'shouldAlwaysBeAvailableMethod' must be as available as declaration it overrides}}
  }
  
  @available(OSX, introduced=10.10)
  override var shouldAlwaysBeAvailableProperty: Int { // expected-error {{overriding 'shouldAlwaysBeAvailableProperty' must be as available as declaration it overrides}}
    get { return 10 }
    set(newVal) {}
  }

  override var setterShouldAlwaysBeAvailableProperty: Int {
    get { return 9 }
    @available(OSX, introduced=10.10)
    set(newVal) {} // expected-error {{overriding '_' must be as available as declaration it overrides}}
    // This is a terrible diagnostic. rdar://problem/20427938
  }

  override var getterShouldAlwaysBeAvailableProperty: Int {
    @available(OSX, introduced=10.10)
    get { return 9 } // expected-error {{overriding '_' must be as available as declaration it overrides}}
    set(newVal) {}
  }
}

class SuperWithLimitedMemberAvailability {
  @available(OSX, introduced=10.10)
  func someMethod() {
  }
  
  @available(OSX, introduced=10.10)
  var someProperty: Int {
    get { return 10 }
    set(newVal) {}
  }
}

class SubWithLargerMemberAvailability : SuperWithLimitedMemberAvailability {
  @available(OSX, introduced=10.9)
  override func someMethod() {
    super.someMethod() // expected-error {{'someMethod()' is only available on OS X 10.10 or newer}}
        // expected-note@-1 {{add @available attribute to enclosing class}}
        // expected-note@-2 {{guard with version check}}
    
    if #available(OSX 10.10, *) {
      super.someMethod()
    }
  }
  
  @available(OSX, introduced=10.9)
  override var someProperty: Int {
    get { 
      let _ = super.someProperty // expected-error {{'someProperty' is only available on OS X 10.10 or newer}}
          // expected-note@-1 {{add @available attribute to enclosing class}}
          // expected-note@-2 {{guard with version check}}
      
      if #available(OSX 10.10, *) {
        let _ = super.someProperty
      }
      
      return 9
      }
    set(newVal) {}
  }
}

// Inheritance and availability

@available(OSX, introduced=10.10)
protocol ProtocolAvailableOn10_10 {
}

@available(OSX, introduced=10.9)
class SubclassAvailableOn10_9OfClassAvailableOn10_10 : ClassAvailableOn10_10 { // expected-error {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
}

@available(OSX, introduced=10.9)
class ClassAvailableOn10_9AdoptingProtocolAvailableOn10_10 : ProtocolAvailableOn10_10 { // expected-error {{'ProtocolAvailableOn10_10' is only available on OS X 10.10 or newer}}
}

// Extensions

extension ClassAvailableOn10_10 { } // expected-error {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
    // expected-note@-1 {{add @available attribute to enclosing extension}}

@available(OSX, introduced=10.10)
extension ClassAvailableOn10_10 {
  func m() {
    let _ = globalFuncAvailableOn10_10()
    let _ = globalFuncAvailableOn10_11() // expected-error {{'globalFuncAvailableOn10_11()' is only available on OS X 10.11 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing instance method}}
      // expected-note@-2 {{guard with version check}}
  }
}

class ClassToExtend { }

@available(OSX, introduced=10.10)
extension ClassToExtend {

  func extensionMethod() { }

  @available(OSX, introduced=10.11)
  func extensionMethod10_11() { }

  class ExtensionClass { }

  // We rely on not allowing nesting of extensions, so test to make sure
  // this emits an error.
  // CHECK:error: declaration is only valid at file scope
  extension ClassToExtend { } // expected-error {{declaration is only valid at file scope}}
}


@available(OSX, introduced=10.10)
extension ClassToExtend { // expected-note {{enclosing scope here}}
  @available(OSX, introduced=10.9) // expected-error {{declaration cannot be more available than enclosing scope}}
  func extensionMethod10_9() { }
}

func useUnavailableExtension() {
  let o = ClassToExtend()

  o.extensionMethod() // expected-error {{'extensionMethod()' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}

  let _ = ClassToExtend.ExtensionClass() // expected-error {{'ExtensionClass' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}

  o.extensionMethod10_11() // expected-error {{'extensionMethod10_11()' is only available on OS X 10.11 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}
}

// Useless #available(...) checks

func functionWithDefaultAvailabilityAndUselessCheck() {
// Default availability reflects minimum deployment: 10.9 and up

  if #available(OSX 10.9, *) { // expected-warning {{unnecessary check for 'OSX'; minimum deployment target ensures guard will always be true}}
    let _ = globalFuncAvailableOn10_9()
  }
  
  if #available(OSX 10.10, *) { // expected-note {{enclosing scope here}}
    let _ = globalFuncAvailableOn10_10()
    
    if #available(OSX 10.10, *) { // expected-warning {{unnecessary check for 'OSX'; enclosing scope ensures guard will always be true}}
      let _ = globalFuncAvailableOn10_10()
    }
  }

  // We don't want * generate a warn about useless checks; the check may be required on
  // another platform
  if #available(iOS 8.0, *) {
  }

  if #available(OSX 10.10, *) {
    // Similarly do not want '*' to generate a warning in a refined TRC.
    if #available(iOS 8.0, *) {
    }
  }
}

@available(OSX, introduced=10.10)
func functionWithSpecifiedAvailabilityAndUselessCheck() { // expected-note 2{{enclosing scope here}}
  if #available(OSX 10.9, *) { // expected-warning {{unnecessary check for 'OSX'; enclosing scope ensures guard will always be true}}
    let _ = globalFuncAvailableOn10_9()
  }
  
  if #available(OSX 10.10, *) { // expected-warning {{unnecessary check for 'OSX'; enclosing scope ensures guard will always be true}}
    let _ = globalFuncAvailableOn10_10()
  }
}

// #available(...) outside if statement guards

func injectToOptional<T>(v: T) -> T? {
  return v
}


if let _ = injectToOptional(5), #available(OSX 10.11, *) {}  // ok


// Refining context inside guard

if #available(OSX 10.10, *),
   let _ = injectToOptional(globalFuncAvailableOn10_10()),
   let _ = injectToOptional(globalFuncAvailableOn10_11()) { // expected-error {{'globalFuncAvailableOn10_11()' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{guard with version check}}

  let _ = globalFuncAvailableOn10_10()
  let _ = globalFuncAvailableOn10_11() // expected-error {{'globalFuncAvailableOn10_11()' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{guard with version check}}
}

if let _ = injectToOptional(5), #available(OSX 10.10, *),
   let _ = injectToOptional(globalFuncAvailableOn10_10()),
   let _ = injectToOptional(globalFuncAvailableOn10_11()) { // expected-error {{'globalFuncAvailableOn10_11()' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{guard with version check}}

  let _ = globalFuncAvailableOn10_10()
  let _ = globalFuncAvailableOn10_11() // expected-error {{'globalFuncAvailableOn10_11()' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{guard with version check}}
}

if let _ = injectToOptional(globalFuncAvailableOn10_10()), #available(OSX 10.10, *), // expected-error {{'globalFuncAvailableOn10_10()' is only available on OS X 10.10 or newer}}
        // expected-note@-1 {{guard with version check}}
   let _ = injectToOptional(globalFuncAvailableOn10_11()) { // expected-error {{'globalFuncAvailableOn10_11()' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{guard with version check}}

}

if let _ = injectToOptional(5), #available(OSX 10.10, *), // expected-note {{enclosing scope here}}
   let _ = injectToOptional(6), #available(OSX 10.10, *) { // expected-warning {{unnecessary check for 'OSX'; enclosing scope ensures guard will always be true}}
}


// Tests for the guard control construct.

func useGuardAvailable() {
  // Guard fallthrough should refine context
  guard #available(OSX 10.10, *) else { // expected-note {{enclosing scope here}}
    let _ = globalFuncAvailableOn10_10() // expected-error {{'globalFuncAvailableOn10_10()' is only available on OS X 10.10 or newer}}
        // expected-note@-1 {{guard with version check}}
        // expected-note@-2 {{add @available attribute to enclosing global function}}
    return
  }

  let _ = globalFuncAvailableOn10_10()

  let _ = globalFuncAvailableOn10_11() // expected-error {{'globalFuncAvailableOn10_11()' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{guard with version check}}
        // expected-note@-2 {{add @available attribute to enclosing global function}}

  if #available(OSX 10.10, *) { // expected-warning {{unnecessary check for 'OSX'; enclosing scope ensures guard will always be true}}
  }

  if globalFuncAvailableOn10_10() > 0 {
    guard #available(OSX 10.11, *),
            let x = injectToOptional(globalFuncAvailableOn10_11()) else { return }
    _ = x
  }

  let _ = globalFuncAvailableOn10_11() // expected-error {{'globalFuncAvailableOn10_11()' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{guard with version check}}
        // expected-note@-2 {{add @available attribute to enclosing global function}}

}

// Refining while loops

while globalFuncAvailableOn10_10() > 10 { } // expected-error {{'globalFuncAvailableOn10_10()' is only available on OS X 10.10 or newer}}
        // expected-note@-1 {{guard with version check}}

while #available(OSX 10.10, *), // expected-note {{enclosing scope here}}
      globalFuncAvailableOn10_10() > 10 {

  let _ = globalFuncAvailableOn10_10()

  let _ = globalFuncAvailableOn10_11() // expected-error {{'globalFuncAvailableOn10_11()' is only available on OS X 10.11 or newer}}
        // expected-note@-1 {{guard with version check}}

  while globalFuncAvailableOn10_10() > 11,
        let _ = injectToOptional(5),
        #available(OSX 10.11, *) {
    let _ = globalFuncAvailableOn10_11();
  }

  while #available(OSX 10.10, *) { // expected-warning {{unnecessary check for 'OSX'; enclosing scope ensures guard will always be true}}
  }
}

// Tests for Fix-It replacement text
// The whitespace in the replacement text is particularly important here -- it reflects the level
// of indentation for the added if #available() or @available attribute. Note that, for the moment, we hard
// code *added* indentation in Fix-Its as 4 spaces (that is, when indenting in a Fix-It, we
// take whatever indentation was there before and add 4 spaces to it).

functionAvailableOn10_10()
    // expected-error@-1 {{'functionAvailableOn10_10()' is only available on OS X 10.10 or newer}}
    // expected-note@-2 {{guard with version check}} {{1-27=if #available(OSX 10.10, *) {\n    functionAvailableOn10_10()\n} else {\n    // Fallback on earlier versions\n}}}

let declForFixitAtTopLevel: ClassAvailableOn10_10? = nil
      // expected-error@-1 {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-2 {{guard with version check}} {{1-57=if #available(OSX 10.10, *) {\n    let declForFixitAtTopLevel: ClassAvailableOn10_10? = nil\n} else {\n    // Fallback on earlier versions\n}}}

func fixitForReferenceInGlobalFunction() {
  functionAvailableOn10_10()
      // expected-error@-1 {{'functionAvailableOn10_10()' is only available on OS X 10.10 or newer}}
      // expected-note@-2 {{guard with version check}} {{3-29=if #available(OSX 10.10, *) {\n      functionAvailableOn10_10()\n  } else {\n      // Fallback on earlier versions\n  }}}
      // expected-note@-3 {{add @available attribute to enclosing global function}} {{1-1=@available(OSX, introduced=10.10)\n}}
}

public func fixitForReferenceInGlobalFunctionWithDeclModifier() {
  functionAvailableOn10_10()
      // expected-error@-1 {{'functionAvailableOn10_10()' is only available on OS X 10.10 or newer}}
      // expected-note@-2 {{guard with version check}} {{3-29=if #available(OSX 10.10, *) {\n      functionAvailableOn10_10()\n  } else {\n      // Fallback on earlier versions\n  }}}
      // expected-note@-3 {{add @available attribute to enclosing global function}} {{1-1=@available(OSX, introduced=10.10)\n}}
}

@noreturn
func fixitForReferenceInGlobalFunctionWithAttribute() {
  functionAvailableOn10_10()
    // expected-error@-1 {{'functionAvailableOn10_10()' is only available on OS X 10.10 or newer}}
    // expected-note@-2 {{guard with version check}} {{3-29=if #available(OSX 10.10, *) {\n      functionAvailableOn10_10()\n  } else {\n      // Fallback on earlier versions\n  }}}
    // expected-note@-3 {{add @available attribute to enclosing global function}} {{1-1=@available(OSX, introduced=10.10)\n}}
}

func takesAutoclosure(@autoclosure c : () -> ()) {
}

class ClassForFixit {
  func fixitForReferenceInMethod() {
    functionAvailableOn10_10()
        // expected-error@-1 {{'functionAvailableOn10_10()' is only available on OS X 10.10 or newer}}
        // expected-note@-2 {{guard with version check}} {{5-31=if #available(OSX 10.10, *) {\n        functionAvailableOn10_10()\n    } else {\n        // Fallback on earlier versions\n    }}}
        // expected-note@-3 {{add @available attribute to enclosing instance method}} {{3-3=@available(OSX, introduced=10.10)\n  }}
        // expected-note@-4 {{add @available attribute to enclosing class}} {{1-1=@available(OSX, introduced=10.10)\n}}
  }

  func fixitForReferenceNestedInMethod() {
    func inner() {
      functionAvailableOn10_10()
          // expected-error@-1 {{'functionAvailableOn10_10()' is only available on OS X 10.10 or newer}}
          // expected-note@-2 {{guard with version check}} {{7-33=if #available(OSX 10.10, *) {\n          functionAvailableOn10_10()\n      } else {\n          // Fallback on earlier versions\n      }}}
          // expected-note@-3 {{add @available attribute to enclosing instance method}} {{3-3=@available(OSX, introduced=10.10)\n  }}
          // expected-note@-4 {{add @available attribute to enclosing class}} {{1-1=@available(OSX, introduced=10.10)\n}}
    }

    let _: () -> () = { () in
      functionAvailableOn10_10()
          // expected-error@-1 {{'functionAvailableOn10_10()' is only available on OS X 10.10 or newer}}
          // expected-note@-2 {{guard with version check}} {{7-33=if #available(OSX 10.10, *) {\n          functionAvailableOn10_10()\n      } else {\n          // Fallback on earlier versions\n      }}}
          // expected-note@-3 {{add @available attribute to enclosing instance method}} {{3-3=@available(OSX, introduced=10.10)\n  }}
          // expected-note@-4 {{add @available attribute to enclosing class}} {{1-1=@available(OSX, introduced=10.10)\n}}
    }

    takesAutoclosure(functionAvailableOn10_10())
          // expected-error@-1 {{'functionAvailableOn10_10()' is only available on OS X 10.10 or newer}}
          // expected-note@-2 {{guard with version check}} {{5-49=if #available(OSX 10.10, *) {\n        takesAutoclosure(functionAvailableOn10_10())\n    } else {\n        // Fallback on earlier versions\n    }}}
          // expected-note@-3 {{add @available attribute to enclosing instance method}} {{3-3=@available(OSX, introduced=10.10)\n  }}
          // expected-note@-4 {{add @available attribute to enclosing class}} {{1-1=@available(OSX, introduced=10.10)\n}}
  }

  var fixitForReferenceInPropertyAccessor: Int {
    get {
      functionAvailableOn10_10()
        // expected-error@-1 {{'functionAvailableOn10_10()' is only available on OS X 10.10 or newer}}
        // expected-note@-2 {{guard with version check}} {{7-33=if #available(OSX 10.10, *) {\n          functionAvailableOn10_10()\n      } else {\n          // Fallback on earlier versions\n      }}}
        // expected-note@-3 {{add @available attribute to enclosing var}} {{3-3=@available(OSX, introduced=10.10)\n  }}
        // expected-note@-4 {{add @available attribute to enclosing class}} {{1-1=@available(OSX, introduced=10.10)\n}}

      return 5
    }
  }

  var fixitForReferenceInPropertyType: ClassAvailableOn10_10? = nil
      // expected-error@-1 {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-2 {{add @available attribute to enclosing class}} {{1-1=@available(OSX, introduced=10.10)\n}}

  // We should really suggest a Fix-It adding @available() to the lazy var.
  // rdar://problem/20968204
  lazy var fixitForReferenceInLazyPropertyType: ClassAvailableOn10_10? = nil
      // expected-error@-1 {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-2 {{add @available attribute to enclosing class}} {{1-1=@available(OSX, introduced=10.10)\n}}

  static var fixitForReferenceInLazyPropertyType: ClassAvailableOn10_10? = nil
      // expected-error@-1 {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-2 {{add @available attribute to enclosing class var}} {{3-3=@available(OSX, introduced=10.10)\n  }}
      // expected-note@-3 {{add @available attribute to enclosing class}} {{1-1=@available(OSX, introduced=10.10)\n}}

  var fixitForReferenceInPropertyTypeMultiple: ClassAvailableOn10_10? = nil, other: Int = 7
      // expected-error@-1 {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-2 {{add @available attribute to enclosing class}} {{1-1=@available(OSX, introduced=10.10)\n}}

  func fixitForRefInGuardOfIf() {
    if (globalFuncAvailableOn10_10() > 1066) {
      let _ = 5
      let _ = 6
    }
        // expected-error@-4 {{'globalFuncAvailableOn10_10()' is only available on OS X 10.10 or newer}}
        // expected-note@-5 {{guard with version check}} {{5-6=if #available(OSX 10.10, *) {\n        if (globalFuncAvailableOn10_10() > 1066) {\n          let _ = 5\n          let _ = 6\n        }\n    } else {\n        // Fallback on earlier versions\n    }}}
        // expected-note@-6 {{add @available attribute to enclosing instance method}} {{3-3=@available(OSX, introduced=10.10)\n  }}
        // expected-note@-7 {{add @available attribute to enclosing class}} {{1-1=@available(OSX, introduced=10.10)\n}}
  }
}

extension ClassToExtend {
  func fixitForReferenceInExtensionMethod() {
    functionAvailableOn10_10()
        // expected-error@-1 {{'functionAvailableOn10_10()' is only available on OS X 10.10 or newer}}
        // expected-note@-2 {{guard with version check}} {{5-31=if #available(OSX 10.10, *) {\n        functionAvailableOn10_10()\n    } else {\n        // Fallback on earlier versions\n    }}}
        // expected-note@-3 {{add @available attribute to enclosing instance method}} {{3-3=@available(OSX, introduced=10.10)\n  }}
        // expected-note@-4 {{add @available attribute to enclosing extension}} {{1-1=@available(OSX, introduced=10.10)\n}}
  }
}

enum EnumForFixit {
  case CaseWithUnavailablePayload(p: ClassAvailableOn10_10)
      // expected-error@-1 {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-2 {{add @available attribute to enclosing case}} {{3-3=@available(OSX, introduced=10.10)\n  }}
      // expected-note@-3 {{add @available attribute to enclosing enum}} {{1-1=@available(OSX, introduced=10.10)\n}}

  case CaseWithUnavailablePayload2(p: ClassAvailableOn10_10), WithoutPayload
      // expected-error@-1 {{'ClassAvailableOn10_10' is only available on OS X 10.10 or newer}}
      // expected-note@-2 {{add @available attribute to enclosing case}} {{3-3=@available(OSX, introduced=10.10)\n  }}
      // expected-note@-3 {{add @available attribute to enclosing enum}} {{1-1=@available(OSX, introduced=10.10)\n}}
}

@objc
class Y {
 var z = 0
}

@objc
class X {
  var y = Y()
}

func testForFixitWithNestedMemberRefExpr() {
  let x = X()

  x.y.z = globalFuncAvailableOn10_11()
      // expected-error@-1 {{'globalFuncAvailableOn10_11()' is only available on OS X 10.11 or newer}}
      // expected-note@-2 {{guard with version check}} {{3-39=if #available(OSX 10.11, *) {\n      x.y.z = globalFuncAvailableOn10_11()\n  } else {\n      // Fallback on earlier versions\n  }}}
      // expected-note@-3 {{add @available attribute to enclosing global function}} {{1-1=@available(OSX, introduced=10.11)\n}}

  // Access via dynamic member reference
  let anyX: AnyObject = x
  anyX.y?.z = globalFuncAvailableOn10_11()
      // expected-error@-1 {{'globalFuncAvailableOn10_11()' is only available on OS X 10.11 or newer}}
      // expected-note@-2 {{guard with version check}} {{3-43=if #available(OSX 10.11, *) {\n      anyX.y?.z = globalFuncAvailableOn10_11()\n  } else {\n      // Fallback on earlier versions\n  }}}
      // expected-note@-3 {{add @available attribute to enclosing global function}} {{1-1=@available(OSX, introduced=10.11)\n}}
}

// Protocol Conformances

protocol HasMethodF {
  typealias T
  func f(p: T) // expected-note 5{{protocol requirement here}}
}

class TriesToConformWithFunctionIntroducedOn10_10 : HasMethodF {
  @available(OSX, introduced=10.10)
  func f(p: Int) { } // expected-error {{'f' is less available than protocol requires}}
}


class ConformsWithFunctionIntroducedOnMinimumDeploymentTarget : HasMethodF {
  // Even though this function is less available than its requirement,
  // it is available on a deployment targets, so the conformance is safe.
  @available(OSX, introduced=10.9)
  func f(p: Int) { }
}

class SuperHasMethodF {
  @available(OSX, introduced=10.10)
    func f(p: Int) { } // expected-error {{'f' is less available than protocol requires}}
}

class TriesToConformWithUnavailableFunctionInSuperClass : SuperHasMethodF, HasMethodF {
  // The conformance here is generating an error on f in the super class -- but
  // there is no indication that the error there is relating to this conformance
  // here.
  // This is a terrible diagnostic. rdar://problem/20610411 tracks improving this.
}

@available(OSX, introduced=10.10)
class ConformsWithUnavailableFunctionInSuperClass : SuperHasMethodF, HasMethodF {
  // Limiting this class to only be available on 10.10 and newer means that
  // the witness in SuperHasMethodF is safe for the requirement on HasMethodF.
  // in order for this class to be referenced we must be running on 10.10 or
  // greater.
}

class ConformsByOverriddingFunctionInSuperClass : SuperHasMethodF, HasMethodF {
  // Now the witness is this f() (which is always available) and not the f()
  // from the super class, so conformance is safe.
  override func f(p: Int) { }
}


// Attempt to conform in protocol extension with unavailable witness
// in extension
class HasNoMethodF1 { }
extension HasNoMethodF1 : HasMethodF {
  @available(OSX, introduced=10.10)
  func f(p: Int) { } // expected-error {{'f' is less available than protocol requires}}
}

class HasNoMethodF2 { }
@available(OSX, introduced=10.10)
extension HasNoMethodF2 : HasMethodF {
  func f(p: Int) { } // expected-error {{'f' is less available than protocol requires}}
}

@available(OSX, introduced=10.10)
class HasNoMethodF3 { }
@available(OSX, introduced=10.10)
extension HasNoMethodF3 : HasMethodF {
  // We expect this conformance to succeed because on every version where HasNoMethodF3
  // is available, HasNoMethodF3's f() is as available as the protocol requirement
  func f(p: Int) { }
}


// We do not want potential unavailability to play a role in picking a witness for a
// protocol requirement. Rather, the witness should be chosen, regardless of its
// potential unavailability, and then it should be diagnosed if it is less available
// than the protocol requires.
class TestAvailabilityDoesNotAffectWitnessCandidacy : HasMethodF {
  // Test that we choose the more specialized witness even though it is
  // less available than the protocol requires and there is a less specialized
  // witness that has suitable availability.

  @available(OSX, introduced=10.10)
  func f(p: Int) { } // expected-error {{'f' is less available than protocol requires}}

  func f<T>(p: T) { }
}

protocol HasUnavailableMethodF {
  @available(OSX, introduced=10.10)
  func f(p: String)
}

class ConformsWithUnavailableFunction : HasUnavailableMethodF {
  @available(OSX, introduced=10.9)
  func f(p: String) { }
}

func useUnavailableProtocolMethod(h: HasUnavailableMethodF) {
  h.f("Foo") // expected-error {{'f' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}
}

func useUnavailableProtocolMethod<H : HasUnavailableMethodF> (h: H) {
  h.f("Foo") // expected-error {{'f' is only available on OS X 10.10 or newer}}
      // expected-note@-1 {{add @available attribute to enclosing global function}}
      // expected-note@-2 {{guard with version check}}
}
