// RUN: %target-parse-verify-swift -enable-experimental-availability-checking

// REQUIRES: OS=macosx

@availability(OSX, introduced=10.9)
var globalAvailableOn10_9: Int = 9

@availability(OSX, introduced=10.10)
var globalAvailableOn10_10: Int = 10

@availability(OSX, introduced=10.11)
var globalAvailableOn10_11: Int = 11

// Top level should reflect the minimum deployment target.
let ignored1: Int = globalAvailableOn10_9

let ignored2: Int = globalAvailableOn10_10 // expected-error {{'globalAvailableOn10_10' is only available on OS X version 10.10 or greater}}
let ignored3: Int = globalAvailableOn10_11 // expected-error {{'globalAvailableOn10_11' is only available on OS X version 10.11 or greater}}

// Functions without annotations should reflect the minimum deployment target.
func functionWithoutAvailability() {
	let _: Int = globalAvailableOn10_9
	
	let _: Int = globalAvailableOn10_10 // expected-error {{'globalAvailableOn10_10' is only available on OS X version 10.10 or greater}}
	let _: Int = globalAvailableOn10_11 // expected-error {{'globalAvailableOn10_11' is only available on OS X version 10.11 or greater}}
}

// Functions with annotations should refine their bodies.
@availability(OSX, introduced=10.10)
func functionAvailableOn10_10() {
	let _: Int = globalAvailableOn10_9
 	let _: Int = globalAvailableOn10_10
 	
 	// Nested functions should get their own refinement context.
 	@availability(OSX, introduced=10.11)
 	func innerFunctionAvailableOn10_11() {
 		let _: Int = globalAvailableOn10_9
 		let _: Int = globalAvailableOn10_10
 		let _: Int = globalAvailableOn10_11
 	}
 	
 	let _: Int = globalAvailableOn10_11 // expected-error {{'globalAvailableOn10_11' is only available on OS X version 10.11 or greater}}
}

if #os(OSX >= 10.10) {
  let _: Int = globalAvailableOn10_10
  let _: Int = globalAvailableOn10_11 // expected-error {{'globalAvailableOn10_11' is only available on OS X version 10.11 or greater}}
}

if #os(OSX >= 10.10) {
  let _: Int = globalAvailableOn10_10
  let _: Int = globalAvailableOn10_11 // expected-error {{'globalAvailableOn10_11' is only available on OS X version 10.11 or greater}}
} else {
  let _: Int = globalAvailableOn10_9
  let _: Int = globalAvailableOn10_10 // expected-error {{'globalAvailableOn10_10' is only available on OS X version 10.10 or greater}}
}

@availability(OSX, introduced=10.10)
var globalAvailableOnOSX10_10AndiOS8_0: Int = 10

if #os(OSX >= 10.10, iOS >= 8.0) {
  let _: Int = globalAvailableOnOSX10_10AndiOS8_0
}

if #os(OSX >= 10.10, OSX >= 10.11) {  // expected-error {{conditions for 'OSX' already specified for this query}}
}

if #os(iOS >= 9.0) {  // expected-error {{condition required for target platform 'OSX'}}
  let _: Int = globalAvailableOnOSX10_10AndiOS8_0 // expected-error {{'globalAvailableOnOSX10_10AndiOS8_0' is only available on OS X version 10.10 or greater}}
}

// Multiple unavailable references in a single statement

let ignored4: (Int, Int) = (globalAvailableOn10_10, globalAvailableOn10_11) // expected-error {{'globalAvailableOn10_10' is only available on OS X version 10.10 or greater}}  expected-error {{'globalAvailableOn10_11' is only available on OS X version 10.11 or greater}}

// Global functions

@availability(OSX, introduced=10.9)
func funcAvailableOn10_9() {}

@availability(OSX, introduced=10.10)
func funcAvailableOn10_10() {}

funcAvailableOn10_9()

let ignored5 = funcAvailableOn10_10 // expected-error {{'funcAvailableOn10_10()' is only available on OS X version 10.10 or greater}}

funcAvailableOn10_10() // expected-error {{'funcAvailableOn10_10()' is only available on OS X version 10.10 or greater}}

if #os(OSX >= 10.10) {
  funcAvailableOn10_10()
}

// Overloaded global functions
@availability(OSX, introduced=10.9)
func overloadedFunction() {}

@availability(OSX, introduced=10.10)
func overloadedFunction(on1010: Int) {}

overloadedFunction()
overloadedFunction(0) // expected-error {{'overloadedFunction' is only available on OS X version 10.10 or greater}}

// Unavailable methods

class ClassWithUnavailableMethod {
  @availability(OSX, introduced=10.9)
  func methAvailableOn10_9() {}
  
  @availability(OSX, introduced=10.10)
  func methAvailableOn10_10() {}
  
  @availability(OSX, introduced=10.10)
  class func classMethAvailableOn10_10() {}
  
  func someOtherMethod() {
    methAvailableOn10_9()
    methAvailableOn10_10() // expected-error {{'methAvailableOn10_10()' is only available on OS X version 10.10 or greater}}
  }
}

func callUnavailableMethods(o: ClassWithUnavailableMethod) {
  let m10_9 = o.methAvailableOn10_9
  m10_9()
  
  let m10_10 = o.methAvailableOn10_10 // expected-error {{'methAvailableOn10_10()' is only available on OS X version 10.10 or greater}}
  m10_10()
  
  o.methAvailableOn10_9()
  o.methAvailableOn10_10() // expected-error {{'methAvailableOn10_10()' is only available on OS X version 10.10 or greater}}
}

func callUnavailableMethodsViaIUO(o: ClassWithUnavailableMethod!) {
  let m10_9 = o.methAvailableOn10_9
  m10_9()
  
  let m10_10 = o.methAvailableOn10_10 // expected-error {{'methAvailableOn10_10()' is only available on OS X version 10.10 or greater}}
  m10_10()
  
  o.methAvailableOn10_9()
  o.methAvailableOn10_10() // expected-error {{'methAvailableOn10_10()' is only available on OS X version 10.10 or greater}}
}

func callUnavailableClassMethod() {
  ClassWithUnavailableMethod.classMethAvailableOn10_10() // expected-error {{'classMethAvailableOn10_10()' is only available on OS X version 10.10 or greater}}
  
  let m10_10 = ClassWithUnavailableMethod.classMethAvailableOn10_10 // expected-error {{'classMethAvailableOn10_10()' is only available on OS X version 10.10 or greater}}
  m10_10()
}

class SubClassWithUnavailableMethod : ClassWithUnavailableMethod {
  func someMethod() {
    methAvailableOn10_9()
    methAvailableOn10_10() // expected-error {{'methAvailableOn10_10()' is only available on OS X version 10.10 or greater}}
  }
}

class SubClassOverridingUnavailableMethod : ClassWithUnavailableMethod {

  override func methAvailableOn10_10() {
    methAvailableOn10_9()
    super.methAvailableOn10_10() // expected-error {{'methAvailableOn10_10()' is only available on OS X version 10.10 or greater}}
    
    let m10_9 = super.methAvailableOn10_9
    m10_9()
    
    let m10_10 = super.methAvailableOn10_10 // expected-error {{'methAvailableOn10_10()' is only available on OS X version 10.10 or greater}}
    m10_10()
  }
  
  func someMethod() {
    methAvailableOn10_9()
    // Calling our override should be fine
    methAvailableOn10_10()
  }
}

class ClassWithUnavailableOverloadedMethod {
  @availability(OSX, introduced=10.9)
  func overloadedMethod() {}

  @availability(OSX, introduced=10.10)
  func overloadedMethod(on1010: Int) {}
}

func callUnavailableOverloadedMethod(o: ClassWithUnavailableOverloadedMethod) {
  o.overloadedMethod()
  o.overloadedMethod(0) // expected-error {{'overloadedMethod' is only available on OS X version 10.10 or greater}}
}

// Initializers

class ClassWithUnavailableInitializer {
  @availability(OSX, introduced=10.9)
  required init() {  }
  
  @availability(OSX, introduced=10.10)
  required init(_ val: Int) {  }
  
  convenience init(s: String) {
    self.init(5) // expected-error {{'init' is only available on OS X version 10.10 or greater}}
  }
  
  @availability(OSX, introduced=10.10)
  convenience init(onlyOn1010: String) {
    self.init(5)
  }
}

func callUnavailableInitializer() {
  ClassWithUnavailableInitializer()
  ClassWithUnavailableInitializer(5) // expected-error {{'init' is only available on OS X version 10.10 or greater}}
  
  let i = ClassWithUnavailableInitializer.self 
  i()
  i(5) // expected-error {{'init' is only available on OS X version 10.10 or greater}}
}

class SuperWithWithUnavailableInitializer {
  @availability(OSX, introduced=10.9)
  init() {  }
  
  @availability(OSX, introduced=10.10)
  init(_ val: Int) {  }
}

class SubOfClassWithUnavailableInitializer : SuperWithWithUnavailableInitializer {
  override init(_ val: Int) {
    super.init(5) // expected-error {{'init' is only available on OS X version 10.10 or greater}}
  }
  
  override init() {
    super.init()
  }
  
  @availability(OSX, introduced=10.10)
  init(on1010: Int) {
    super.init(22)
  }
}

// Properties

class ClassWithUnavailableProperties {
  @availability(OSX, introduced=10.9)
  var availableOn10_9Stored: Int = 9
  
  @availability(OSX, introduced=10.10)
  var availableOn10_10Stored : Int = 10

  @availability(OSX, introduced=10.9)
  var availableOn10_9Computed: Int {
    get {
      let _: Int = availableOn10_10Stored // expected-error {{'availableOn10_10Stored' is only available on OS X version 10.10 or greater}}
      
      if #os(OSX >= 10.10) {
        let _: Int = availableOn10_10Stored
      }
      
      return availableOn10_9Stored
    }
    set(newVal) {
      availableOn10_9Stored = newVal
    }
  }
  
  @availability(OSX, introduced=10.10)
  var availableOn10_10Computed: Int {
    get {
      return availableOn10_10Stored
    }
    set(newVal) {
      availableOn10_10Stored = newVal
    }
  }
}

func accessUnavailableProperties(o: ClassWithUnavailableProperties) {
  // Stored properties
  let _: Int = o.availableOn10_9Stored
  let _: Int = o.availableOn10_10Stored // expected-error {{'availableOn10_10Stored' is only available on OS X version 10.10 or greater}}
  
  o.availableOn10_9Stored = 9
  o.availableOn10_10Stored = 10 // expected-error {{'availableOn10_10Stored' is only available on OS X version 10.10 or greater}}
  
  // Computed Properties
  let _: Int = o.availableOn10_9Computed
  let _: Int = o.availableOn10_10Computed // expected-error {{'availableOn10_10Computed' is only available on OS X version 10.10 or greater}}
  
  o.availableOn10_9Computed = 9
  o.availableOn10_10Computed = 10 // expected-error {{'availableOn10_10Computed' is only available on OS X version 10.10 or greater}}
}

// Classes

@availability(OSX, introduced=10.9)
class ClassAvailableOn10_9 {
  func someMethod() {}
  class func someClassMethod() {}
  var someProp : Int = 22
}

@availability(OSX, introduced=10.10)
class ClassAvailableOn10_10 {  
  func someMethod() {}
  class func someClassMethod() {
    let _ = ClassAvailableOn10_10()
  }
  var someProp : Int = 22
}

func classAvailability() {
  ClassAvailableOn10_9.someClassMethod()
  ClassAvailableOn10_10.someClassMethod() // expected-error {{'ClassAvailableOn10_10' is only available on OS X version 10.10 or greater}}

  ClassAvailableOn10_9.self
  ClassAvailableOn10_10.self // expected-error {{'ClassAvailableOn10_10' is only available on OS X version 10.10 or greater}}
  
  let o10_9 = ClassAvailableOn10_9()
  let o10_10 = ClassAvailableOn10_10() // expected-error {{'ClassAvailableOn10_10' is only available on OS X version 10.10 or greater}}
  
  o10_9.someMethod()
  o10_10.someMethod()
  
  let _ = o10_9.someProp
  let _ = o10_10.someProp 
}

func castingUnavailableClass(o : AnyObject) {
  let _ = o as! ClassAvailableOn10_10 // expected-error {{'ClassAvailableOn10_10' is only available on OS X version 10.10 or greater}}
  let _ = o as? ClassAvailableOn10_10 // expected-error {{'ClassAvailableOn10_10' is only available on OS X version 10.10 or greater}}
  let _ = o is ClassAvailableOn10_10 // expected-error {{'ClassAvailableOn10_10' is only available on OS X version 10.10 or greater}}
}

protocol Createable {
  init()
}

@availability(OSX, introduced=10.10)
class ClassAvailableOn10_10_Createable : Createable { 
  required init() {}
}

func create<T : Createable>() -> T {
  return T()
}

func classViaTypeParameter() {
  let _ : ClassAvailableOn10_10_Createable = // expected-error {{'ClassAvailableOn10_10_Createable' is only available on OS X version 10.10 or greater}}
      create() // expected-error {{'ClassAvailableOn10_10_Createable' is only available on OS X version 10.10 or greater}}
      
      
  let _ = [ClassAvailableOn10_10]() // expected-error {{'ClassAvailableOn10_10' is only available on OS X version 10.10 or greater}}
}

// Unavailable class used in declarations

class ClassWithDeclarationsOfUnavailableClasses {

  @availability(OSX, introduced=10.10)
  init() {
    unavailablePropertyOfUnavailableType = ClassAvailableOn10_10()
    unavailablePropertyOfUnavailableType = ClassAvailableOn10_10()
  }

  var propertyOfUnavailableType: ClassAvailableOn10_10 // expected-error {{'ClassAvailableOn10_10' is only available on OS X version 10.10 or greater}}
  
  @availability(OSX, introduced=10.10)
  var unavailablePropertyOfUnavailableType: ClassAvailableOn10_10
  
  @availability(OSX, introduced=10.10)
  var unavailablePropertyOfUnavailableTypeWithInitializer: ClassAvailableOn10_10 = ClassAvailableOn10_10() 
  
  func methodWithUnavailableParameterType(o : ClassAvailableOn10_10) { // expected-error {{'ClassAvailableOn10_10' is only available on OS X version 10.10 or greater}}
  }
  
  @availability(OSX, introduced=10.10)
  func unavailableMethodWithUnavailableParameterType(o : ClassAvailableOn10_10) {
  }
  
  func methodWithUnavailableReturnType() -> ClassAvailableOn10_10  { // expected-error {{'ClassAvailableOn10_10' is only available on OS X version 10.10 or greater}}
    return ClassAvailableOn10_10() // expected-error {{'ClassAvailableOn10_10' is only available on OS X version 10.10 or greater}}
  }
  
  @availability(OSX, introduced=10.10)
  func unavailableMethodWithUnavailableReturnType() -> ClassAvailableOn10_10  {
    return ClassAvailableOn10_10()
  }

  func methodWithUnavailableLocalDeclaration() {
    let o : ClassAvailableOn10_10 = methodWithUnavailableReturnType() // expected-error {{'ClassAvailableOn10_10' is only available on OS X version 10.10 or greater}}
  }
  
  @availability(OSX, introduced=10.10)
  func unavailableMethodWithUnavailableLocalDeclaration() {
    let o : ClassAvailableOn10_10 = methodWithUnavailableReturnType()
  }
}

class ClassExtendingUnavailableClass : ClassAvailableOn10_10 { // expected-error {{'ClassAvailableOn10_10' is only available on OS X version 10.10 or greater}}
}

@availability(OSX, introduced=10.10)
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
}

class SubWithLimitedMemberAvailability : SuperWithAlwaysAvailableMembers {
  @availability(OSX, introduced=10.10)
  override func shouldAlwaysBeAvailableMethod() { // expected-error {{overriding 'shouldAlwaysBeAvailableMethod' must be as available as declaration it overrides}}
  }
  
  @availability(OSX, introduced=10.10)
  override var shouldAlwaysBeAvailableProperty: Int { // expected-error {{overriding 'shouldAlwaysBeAvailableProperty' must be as available as declaration it overrides}}
    get { return 10 }
    set(newVal) {}
  }
}

class SuperWithLimitedMemberAvailability {
  @availability(OSX, introduced=10.10)
  func someMethod() {
  }
  
  @availability(OSX, introduced=10.10)
  var someProperty: Int {
    get { return 10 }
    set(newVal) {}
  }
}

class SubWithLargerMemberAvailability : SuperWithLimitedMemberAvailability {
  @availability(OSX, introduced=10.9)
  override func someMethod() {
    super.someMethod() // expected-error {{'someMethod()' is only available on OS X version 10.10 or greater}}
    
    if #os(OSX >= 10.10) {
      super.someMethod()
    }
  }
  
  @availability(OSX, introduced=10.9)
  override var someProperty: Int {
    get { 
      let _ = super.someProperty // expected-error {{'someProperty' is only available on OS X version 10.10 or greater}}
      
      if #os(OSX >= 10.10) {
        let _ = super.someProperty
      }
      
      return 9
      }
    set(newVal) {}
  }
}

// Inheritance and availability

@availability(OSX, introduced=10.10)
protocol ProtocolAvailableOn10_10 {
}

@availability(OSX, introduced=10.9)
class SubclassAvailableOn10_9OfClassAvailableOn10_10 : ClassAvailableOn10_10 { // expected-error {{'ClassAvailableOn10_10' is only available on OS X version 10.10 or greater}}
}

@availability(OSX, introduced=10.9)
class ClassAvailableOn10_9AdoptingProtocolAvailableOn10_10 : ProtocolAvailableOn10_10 { // expected-error {{'ProtocolAvailableOn10_10' is only available on OS X version 10.10 or greater}}
}

// Useless #os(...) checks

func functionWithDefaultAvailabilityAndUselessCheck() {
// Default availability reflects minimum deployment: 10.9 and up

  if #os(OSX >= 10.9) { // expected-warning {{unnecessary check for 'OSX'; minimum deployment target ensures guard will always be true}}
    let _ = globalAvailableOn10_9
  }
  
  if #os(OSX >= 10.10) { // expected-note {{enclosing scope here}}
    let _ = globalAvailableOn10_10
    
    if #os(OSX >= 10.10) { // expected-warning {{unnecessary check for 'OSX'; enclosing scope ensures guard will always be true}}
      let _ = globalAvailableOn10_10 
    }
  }
}

@availability(OSX, introduced=10.10)
func functionWithSpecifiedAvailabilityAndUselessCheck() { // expected-note 2{{enclosing scope here}}
  if #os(OSX >= 10.9) { // expected-warning {{unnecessary check for 'OSX'; enclosing scope ensures guard will always be true}}
    let _ = globalAvailableOn10_9
  }
  
  if #os(OSX >= 10.10) { // expected-warning {{unnecessary check for 'OSX'; enclosing scope ensures guard will always be true}}
    let _ = globalAvailableOn10_10 
  }
}

// #os(...) outside if statement guards

let _ = #os(OSX >= 10.10) // expected-error {{check can only be used as guard of if statement}}

// For the moment, we don't allow #os() in IfExprs.
(#os(OSX >= 10.10) ? 1 : 0) // expected-error {{check can only be used as guard of if statement}}

if #os(OSX >= 10.10) && #os(OSX >= 10.11) { // expected-error 2{{check can only be used as guard of if statement}}
}
