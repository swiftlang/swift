// RUN: %swift -parse -enable-experimental-availability-checking -target x86_64-apple-macosx10.9 -verify %s

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
