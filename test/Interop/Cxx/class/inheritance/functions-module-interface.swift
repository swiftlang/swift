// RUN: %target-swift-ide-test -print-module -module-to-print=Functions -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK:      struct NonTrivial {
// CHECK-NEXT:   init()
// CHECK-NEXT:   func inNonTrivial() -> UnsafePointer<CChar>!
// CHECK-NEXT:   func inNonTrivialWithArgs(_ a: Int32, _ b: Int32) -> UnsafePointer<CChar>!
// CHECK-NEXT: }

// CHECK-NEXT: struct Base {
// CHECK-NEXT:   init()
// CHECK-NEXT:   mutating func mutatingInBase() -> UnsafePointer<CChar>!
// CHECK-NEXT:   func constInBase() -> UnsafePointer<CChar>!
// CHECK-NEXT:   func takesArgsInBase(_ a: Int32, _ b: Int32, _ c: Int32) -> UnsafePointer<CChar>!
// CHECK-NEXT:   func takesNonTrivialInBase(_ a: NonTrivial) -> UnsafePointer<CChar>!
// CHECK-NEXT:   func returnsNonTrivialInBase() -> NonTrivial
// CHECK-NEXT:   func templateInBase<T>(_ t: T) -> UnsafePointer<CChar>!
// CHECK-NEXT:   static func staticInBase() -> UnsafePointer<CChar>!
// CHECK-NEXT: }

// CHECK-NEXT: struct OtherBase {
// CHECK-NEXT:   init()
// CHECK-NEXT:   func inOtherBase() -> UnsafePointer<CChar>!
// CHECK-NEXT: }

// CHECK-NEXT: struct Derived {
// CHECK-NEXT:   init()
// CHECK-NEXT:   func inDerived() -> UnsafePointer<CChar>!
// CHECK-NEXT:   mutating func mutatingInBase() -> UnsafePointer<CChar>?
// CHECK-NEXT:   func constInBase() -> UnsafePointer<CChar>?
// CHECK-NEXT:   func takesArgsInBase(_ a: Int32, _ b: Int32, _ c: Int32) -> UnsafePointer<CChar>?
// CHECK-NEXT:   func takesNonTrivialInBase(_ a: NonTrivial) -> UnsafePointer<CChar>?
// CHECK-NEXT:   func returnsNonTrivialInBase() -> NonTrivial
// CHECK-NEXT:   func inOtherBase() -> UnsafePointer<CChar>?
// CHECK-NEXT: }

// CHECK-NEXT: struct DerivedFromDerived {
// CHECK-NEXT:   init()
// CHECK-NEXT:   func topLevel() -> UnsafePointer<CChar>!
// CHECK-NEXT:   func inDerived() -> UnsafePointer<CChar>?
// CHECK-NEXT:   mutating func mutatingInBase() -> UnsafePointer<CChar>?
// CHECK-NEXT:   func constInBase() -> UnsafePointer<CChar>?
// CHECK-NEXT:   func takesArgsInBase(_ a: Int32, _ b: Int32, _ c: Int32) -> UnsafePointer<CChar>?
// CHECK-NEXT:   func takesNonTrivialInBase(_ a: NonTrivial) -> UnsafePointer<CChar>?
// CHECK-NEXT:   func returnsNonTrivialInBase() -> NonTrivial
// CHECK-NEXT:   func inOtherBase() -> UnsafePointer<CChar>?
// CHECK-NEXT: }

// CHECK-NEXT: struct DerivedFromNonTrivial {
// CHECK-NEXT:   init()
// CHECK-NEXT:   func inNonTrivial() -> UnsafePointer<CChar>?
// CHECK-NEXT:   func inNonTrivialWithArgs(_ a: Int32, _ b: Int32) -> UnsafePointer<CChar>?
// CHECK-NEXT: }
