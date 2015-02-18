// RUN: %target-parse-verify-swift -enable-experimental-availability-checking -enable-experimental-unavailable-as-optional

// REQUIRES: OS=macosx
// REQUIRES: objc_interop

@availability(OSX, introduced=10.9)
var globalAvailableOn10_9: Int = 9

@availability(OSX, introduced=10.10)
var globalAvailableOn10_10: Int = 10

@availability(OSX, introduced=10.11)
var globalAvailableOn10_11: Int = 11

@availability(OSX, introduced=10.10)
var globalOptionalAvailableOn10_10: Int? = 10

@availability(OSX, introduced=10.10)
var globalIUOptionalAvailableOn10_10: Int! = 10

func referencesToGlobalVariables() {
  // Potentially unavailable symbols should have optional type
  let _: Int = globalAvailableOn10_10 // expected-error {{value of optional type 'Int?' not unwrapped; did you mean to use '!' or '?'?}}

  if let _ = globalAvailableOn10_10 {}
  let _: Int = globalAvailableOn10_10!

  // Definitely available symbols should have their declared type.
  let _: Int = globalAvailableOn10_9
  
  let _: Int = globalAvailableOn10_9! // expected-error {{operand of postfix '!' should have optional type; type is 'Int'}}
  
  // Potentially unavailable declarations with optional types should be doubly optional
  
  let _: Int? =  globalOptionalAvailableOn10_10 // expected-error {{value of optional type 'Int??' not unwrapped; did you mean to use '!' or '?'?}}
  if let level1: Int? = globalOptionalAvailableOn10_10 {
    if let _:   Int = level1 {}
  } 
}

// Multiple unavailable references in a single statement

let ignored1: (Int, Int) = (globalAvailableOn10_10, globalAvailableOn10_11) // expected-error 2{{value of optional type 'Int?' not unwrapped; did you mean to use '!' or '?'?}}

// Global functions

@availability(OSX, introduced=10.9)
func funcAvailableOn10_9() -> Int { return 9 }

@availability(OSX, introduced=10.10)
func funcAvailableOn10_10() -> Int { return 10 }

func referToFunctions() {
  let _: () -> Int = funcAvailableOn10_9
  
  let _: () -> Int = funcAvailableOn10_10 // expected-error {{value of optional type '(() -> Int)?' not unwrapped; did you mean to use '!' or '?'?}}
}

func callFunctions() {
  funcAvailableOn10_9()
  
  funcAvailableOn10_10() // expected-error {{value of optional type '(() -> Int)?' not unwrapped; did you mean to use '!' or '?'?}}
  
  let _: Int = funcAvailableOn10_10!()
  
  let _: Int? = funcAvailableOn10_10() // expected-error {{value of optional type '(() -> Int)?' not unwrapped; did you mean to use '!' or '?'?}}
  
  let _: Int? = funcAvailableOn10_10?()
}

@availability(OSX, introduced=10.9)
func overloadedFunction() -> Int { return 42 }

@availability(OSX, introduced=10.10)
func overloadedFunction(on1010: Int) -> Int { return 43 }

func callOverloadedFunctions() {
  overloadedFunction()
  overloadedFunction(0) // expected-error {{value of optional type '((Int) -> Int)?' not unwrapped; did you mean to use '!' or '?'?}}
  
  let _: Int = overloadedFunction()
  let _: Int = overloadedFunction!(0)
  if let _: Int = overloadedFunction?(0) {}
}

// Unavailable methods

class ClassWithUnavailableMethod {
  @availability(OSX, introduced=10.9)
  func methAvailableOn10_9() -> Int { return 9 }
  
  @availability(OSX, introduced=10.10)
  func methAvailableOn10_10() -> Int { return 10 }
  
  @availability(OSX, introduced=10.10)
  class func classMethAvailableOn10_10() -> Int { return 10 }
  
  func someOtherMethod() {
    methAvailableOn10_9()
    methAvailableOn10_10() // expected-error {{value of optional type '(() -> Int)?' not unwrapped; did you mean to use '!' or '?'?}}
    
    let _: () -> Int = methAvailableOn10_9
    let _: () -> Int = methAvailableOn10_10 // expected-error {{value of optional type '(() -> Int)?' not unwrapped; did you mean to use '!' or '?'?}}
    if let _: () -> Int = methAvailableOn10_10 {}
    
    let _: Int = methAvailableOn10_10!()
    if let _: Int = methAvailableOn10_10?() {}
  }
}

func callUnavailableMethods(o: ClassWithUnavailableMethod) {
  let m10_9 = o.methAvailableOn10_9
  m10_9()
  
  if let m10_10 : () -> Int = o.methAvailableOn10_10 {
    m10_10()
  }
  
  o.methAvailableOn10_9()
  o.methAvailableOn10_10() // expected-error {{value of optional type '(() -> Int)?' not unwrapped; did you mean to use '!' or '?'?}}
  
  let _: Int = o.methAvailableOn10_10!()
  if let _: Int = o.methAvailableOn10_10?() {}
}

func callUnavailableMethodsViaIUO(o: ClassWithUnavailableMethod!) {
  let m10_9 = o.methAvailableOn10_9
  m10_9()
  
  if let m10_10 : () -> Int = o.methAvailableOn10_10 {
    m10_10()
  }
  
  o.methAvailableOn10_9()
  o.methAvailableOn10_10() // expected-error {{value of optional type '(() -> Int)?' not unwrapped; did you mean to use '!' or '?'?}}
  
  let _: Int = o.methAvailableOn10_10!()
  if let _: Int = o.methAvailableOn10_10?() {}
}

func callUnavailableClassMethod() {
  ClassWithUnavailableMethod.classMethAvailableOn10_10() // expected-error {{value of optional type '(() -> Int)?' not unwrapped; did you mean to use '!' or '?'?}}
  
  if let _: () -> Int = ClassWithUnavailableMethod.classMethAvailableOn10_10 {}

  let _: Int = ClassWithUnavailableMethod.classMethAvailableOn10_10!()
  if let _: Int = ClassWithUnavailableMethod.classMethAvailableOn10_10?() {}
}

class ClassWithUnavailableOverloadedMethod {
  @availability(OSX, introduced=10.9)
  func overloadedMethod() -> Int { return 9 }

  @availability(OSX, introduced=10.10)
  func overloadedMethod(on1010: Int) -> Int { return 10 }
}

func callUnavailableOverloadedMethod(o: ClassWithUnavailableOverloadedMethod) {
  o.overloadedMethod()
  o.overloadedMethod(0) // expected-error {{value of optional type '((Int) -> Int)?' not unwrapped; did you mean to use '!' or '?'?}}
  
  let _: Int = o.overloadedMethod!(0)
  if let _: Int = o.overloadedMethod?(0) { } 
}

class ClassWithMethodReturningOptional {
   @availability(OSX, introduced=10.10)
  func methAvailableOn10_10() -> Int? { return 10 }
}

func callMethodReturningOptional(o: ClassWithMethodReturningOptional) {
  let _: Int? = o.methAvailableOn10_10() // expected-error {{value of optional type '(() -> Int?)?' not unwrapped; did you mean to use '!' or '?'?}}
  
  let _: Int? = o.methAvailableOn10_10!()
  let _: Int?? = o.methAvailableOn10_10?()
  
  let _: Int? = o.methAvailableOn10_10?()!
  let _: Int = o.methAvailableOn10_10!()!
}


// Initializers
// For the moment, we do not convert unavailable initializers to optionals
// and instead just diagnose unavailability.
class ClassWithUnavailableInitializer {
  @availability(OSX, introduced=10.10)
  required init(_ v: Int) {  }
  
  convenience init(s: String) {
    self.init(5) // expected-error {{'init' is only available on OS X version 10.10 or greater}}
  }
}

func callUnavailableInitializer() {
  ClassWithUnavailableInitializer(5) // expected-error {{'init' is only available on OS X version 10.10 or greater}}
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
      let _: Int = availableOn10_10Stored // expected-error {{value of optional type 'Int?' not unwrapped; did you mean to use '!' or '?'?}}
      let _: Int = availableOn10_10Stored!
      
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
  let _: Int = o.availableOn10_10Stored // expected-error {{value of optional type 'Int?' not unwrapped; did you mean to use '!' or '?'?}}
  
  let _: Int = o.availableOn10_10Stored!
  
  o.availableOn10_9Stored = 9
  
  // We don't support unavailable optionals as lvals yet.
  o.availableOn10_10Stored = 10 // expected-error {{cannot assign to 'availableOn10_10Stored' in 'o'}}
  
  // Computed Properties
  let _: Int = o.availableOn10_9Computed
  let _: Int = o.availableOn10_10Computed // expected-error {{value of optional type 'Int?' not unwrapped; did you mean to use '!' or '?'?}}
  
  let _: Int = o.availableOn10_10Computed!
  
  o.availableOn10_9Computed = 9
  
  // We don't support unavailable optionals as lvals yet.
  o.availableOn10_10Computed = 10 // expected-error {{cannot assign to 'availableOn10_10Computed' in 'o'}}
}
