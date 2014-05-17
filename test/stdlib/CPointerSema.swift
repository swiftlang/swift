// RUN: %swift -parse -verify %s

// Test that the C*Pointer types support all of the expected conversions.

func takeConstPointer(p: CConstPointer<Int>) {} // expected-note{{}}

var mutable: Int = 0
let immutable: Int = 0
var differentType: Double = 0
var arr: Int[] = [0]
var arrDifferentType: Double[] = [0]
let ptr: UnsafePointer<Int> = nil
let ptrDifferentType: UnsafePointer<Double> = nil

takeConstPointer(nil)
takeConstPointer(&mutable)
takeConstPointer(immutable) // expected-error{{}}
takeConstPointer(&differentType) // expected-error{{}}
takeConstPointer(arr)
takeConstPointer(arrDifferentType) // expected-error{{}}
takeConstPointer([0])
takeConstPointer([0.0]) // expected-error{{}}
takeConstPointer(ptr)
takeConstPointer(ptrDifferentType) // expected-error{{}}

func takeMutablePointer(p: CMutablePointer<Int>) -> Bool { // expected-note{{}}
  // Test '=='
  if p == p { return true }
  if p != p { return false }
  return false
}

takeMutablePointer(nil)
takeMutablePointer(&mutable)
takeMutablePointer(immutable) // expected-error{{}}
takeMutablePointer(&differentType) // expected-error{{}}
takeMutablePointer(arr) // expected-error{{}}
takeMutablePointer(&arr)
takeMutablePointer(&arrDifferentType) // expected-error{{}}
takeMutablePointer([0]) // expected-error{{}}
takeMutablePointer([0.0]) // expected-error{{}}
takeMutablePointer(ptr)
takeMutablePointer(ptrDifferentType) // expected-error{{}}

class NSFoo {}
func takeAutoreleasingUnsafePointer(p: AutoreleasingUnsafePointer<NSFoo>) -> Bool { // expected-note{{}}
  // Test '=='
  if p == p { return true }
  if p != p { return false }
  return false
} 

var mutableFoo = NSFoo()
var fooArr = [mutableFoo]
let immutableFoo = NSFoo()
let fooPtr: UnsafePointer<NSFoo> = nil

takeAutoreleasingUnsafePointer(nil)
takeAutoreleasingUnsafePointer(immutableFoo) // expected-error{{}}
takeAutoreleasingUnsafePointer(fooArr) // expected-error{{}}
takeAutoreleasingUnsafePointer(&fooArr) // expected-error{{}}
takeAutoreleasingUnsafePointer(&mutableFoo)
takeAutoreleasingUnsafePointer(fooPtr)

func takeConstVoidPointer(p: CConstVoidPointer) {} // expected-note{{}}

takeConstVoidPointer(nil)
takeConstVoidPointer(&mutable)
takeConstVoidPointer(immutable) // expected-error{{}}
takeConstVoidPointer(&differentType)
takeConstVoidPointer(arr)
takeConstVoidPointer(arrDifferentType)
takeConstVoidPointer([0])
takeConstVoidPointer([0.0])
takeConstVoidPointer(ptr)
takeConstVoidPointer(ptrDifferentType)

func takeMutableVoidPointer(p: CMutableVoidPointer) {} // expected-note{{}}

takeMutableVoidPointer(nil)
takeMutableVoidPointer(&mutable)
takeMutableVoidPointer(immutable) // expected-error{{}}
takeMutableVoidPointer(&differentType)
takeMutableVoidPointer(arr) // expected-error{{}}
takeMutableVoidPointer(&arr)
takeMutableVoidPointer(&arrDifferentType)
takeMutableVoidPointer([0]) // expected-error{{}}
takeMutableVoidPointer([0.0]) // expected-error{{}}
takeMutableVoidPointer(ptr)
takeMutableVoidPointer(ptrDifferentType)
