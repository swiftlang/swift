// RUN: %target-swift-ide-test -print-module -module-to-print=Fields -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK:      struct HasThreeFields {
// CHECK-NEXT:   init(a: CInt, b: CInt, c: CInt)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var a: CInt
// CHECK-NEXT:   var b: CInt
// CHECK-NEXT:   var c: CInt
// CHECK-NEXT: }

// CHECK-NEXT: struct DerivedWithSameField {
// CHECK-NEXT:   init()
// CHECK-NEXT:   var b: CInt
// CHECK-NEXT:   var c: CInt
// CHECK-NEXT:   var a: CInt
// CHECK-NEXT: }

// CHECK-NEXT: struct DerivedWithOneField {
// CHECK-NEXT:   init()
// CHECK-NEXT:   var a: CInt
// CHECK-NEXT:   var b: CInt
// CHECK-NEXT:   var c: CInt
// CHECK-NEXT:   var d: CInt
// CHECK-NEXT: }

// CHECK-NEXT: struct HasOneField {
// CHECK-NEXT:   init(e: CInt)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var e: CInt
// CHECK-NEXT: }

// CHECK-NEXT: struct DerivedFromAll {
// CHECK-NEXT:   init()
// CHECK-NEXT:   var a: CInt
// CHECK-NEXT:   var b: CInt
// CHECK-NEXT:   var c: CInt
// CHECK-NEXT:   var d: CInt
// CHECK-NEXT:   var e: CInt
// CHECK-NEXT:   var f: CInt
// CHECK-NEXT: }

// CHECK-NEXT: struct OneField {
// CHECK-NEXT:   init(value: CInt)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var value: CInt
// CHECK-NEXT: }
// CHECK-NEXT: struct DerivedFromOneField {
// CHECK-NEXT:   init()
// CHECK-NEXT:   var value: CInt
// CHECK-NEXT: }

// CHECK-NEXT: struct NonTrivial {
// CHECK-NEXT:   init()
// CHECK-NEXT: }

// CHECK-NEXT: struct NonTrivialHasThreeFields {
// CHECK-NEXT:   init(a: CInt, b: CInt, c: CInt)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var a: CInt
// CHECK-NEXT:   var b: CInt
// CHECK-NEXT:   var c: CInt
// CHECK-NEXT: }

// CHECK-NEXT: struct NonTrivialDerivedWithOneField {
// CHECK-NEXT:   init()
// CHECK-NEXT:   var a: CInt
// CHECK-NEXT:   var b: CInt
// CHECK-NEXT:   var c: CInt
// CHECK-NEXT:   var d: CInt
// CHECK-NEXT: }

// CHECK-NEXT: struct NonTrivialHasOneField {
// CHECK-NEXT:   init()
// CHECK-NEXT:   var e: CInt
// CHECK-NEXT: }

// CHECK-NEXT: struct NonTrivialDerivedFromAll {
// CHECK-NEXT:   init()
// CHECK-NEXT:   var a: CInt
// CHECK-NEXT:   var b: CInt
// CHECK-NEXT:   var c: CInt
// CHECK-NEXT:   var d: CInt
// CHECK-NEXT:   var e: CInt
// CHECK-NEXT:   var f: CInt
// CHECK-NEXT: }
