// RUN: %swift -parse -verify %s

// Test that the C*Pointer types support all of the expected conversions.

func takeConstPointer(p: CConstPointer<Int>) {} // expected-note{{}}

var mutable: Int = 0
let immutable: Int = 0
var differentType: Double = 0
var arr: Int[] = [0]
let ptr: UnsafePointer<Int> = nil

takeConstPointer(nil)
takeConstPointer(&mutable)
takeConstPointer(immutable) // expected-error{{}}
takeConstPointer(&differentType) // expected-error{{}}
takeConstPointer(arr)
takeConstPointer([0])
takeConstPointer(ptr)

func takeMutablePointer(p: CMutablePointer<Int>) {} // expected-note{{}}

takeMutablePointer(nil)
takeMutablePointer(&mutable)
takeMutablePointer(immutable) // expected-error{{}}
takeMutablePointer(&differentType) // expected-error{{}}
takeMutablePointer(arr) // expected-error{{}}
takeMutablePointer(&arr)
takeMutablePointer([0]) // expected-error{{}}
takeMutablePointer(ptr)
