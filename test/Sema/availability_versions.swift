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
