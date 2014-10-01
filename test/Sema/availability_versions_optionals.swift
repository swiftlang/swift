// RUN: %swift -parse -enable-experimental-availability-checking -enable-experimental-unavailable-as-optional -target x86_64-apple-macosx10.9 -verify %s

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
  let _: Int? = overloadedFunction?(0)
}
