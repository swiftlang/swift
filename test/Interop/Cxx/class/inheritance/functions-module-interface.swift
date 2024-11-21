// RUN: %target-swift-ide-test -print-module -print-implicit-attrs -module-to-print=Functions -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK:      struct NonTrivial {
// CHECK-NEXT:   init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func inNonTrivial() -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func inNonTrivialWithArgs(_ a: Int32, _ b: Int32) -> UnsafePointer<CChar>!
// CHECK-NEXT: }

// CHECK-NEXT: struct Base {
// CHECK-NEXT:   init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   mutating func mutatingInBase() -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func constInBase() -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func rvalueThisInBase() -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func takesArgsInBase(_ a: Int32, _ b: Int32, _ c: Int32) -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func takesNonTrivialInBase(_ a: NonTrivial) -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func returnsNonTrivialInBase() -> NonTrivial
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func templateInBase<T>(_ t: T) -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   static func staticInBase() -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   mutating func swiftRenamed(input i: Int32) -> Int32
// CHECK-NEXT:   @available(swift, obsoleted: 3, renamed: "swiftRenamed(input:)")
// CHECK-NEXT:   mutating func renamed(_ i: Int32) -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   @_effects(readonly) func pure() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func sameMethodNameSameSignature() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func sameMethodDifferentSignature() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: struct OtherBase {
// CHECK-NEXT:   init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func inOtherBase() -> UnsafePointer<CChar>!
// CHECK-NEXT: }

// CHECK-NEXT: struct Derived {
// CHECK-NEXT:   init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func inDerived() -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func sameMethodNameSameSignature() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func sameMethodDifferentSignature(_ x: Int32) -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   mutating func mutatingInBase() -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func constInBase() -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func takesArgsInBase(_ a: Int32, _ b: Int32, _ c: Int32) -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func takesNonTrivialInBase(_ a: NonTrivial) -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func returnsNonTrivialInBase() -> NonTrivial
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   mutating func swiftRenamed(input i: Int32) -> Int32
// CHECK-NEXT:   @available(swift, obsoleted: 3, renamed: "swiftRenamed(input:)")
// CHECK-NEXT:   mutating func renamed(_ i: Int32) -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   @_effects(readonly) func pure() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func sameMethodDifferentSignature() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func inOtherBase() -> UnsafePointer<CChar>?
// CHECK-NEXT: }

// CHECK-NEXT: struct DerivedFromDerived {
// CHECK-NEXT:   init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func topLevel() -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func inDerived() -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func sameMethodNameSameSignature() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func sameMethodDifferentSignature(_ x: Int32) -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   mutating func mutatingInBase() -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func constInBase() -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func takesArgsInBase(_ a: Int32, _ b: Int32, _ c: Int32) -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func takesNonTrivialInBase(_ a: NonTrivial) -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func returnsNonTrivialInBase() -> NonTrivial
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   mutating func swiftRenamed(input i: Int32) -> Int32
// CHECK-NEXT:   @available(swift, obsoleted: 3, renamed: "swiftRenamed(input:)")
// CHECK-NEXT:   mutating func renamed(_ i: Int32) -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   @_effects(readonly) func pure() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func sameMethodDifferentSignature() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func inOtherBase() -> UnsafePointer<CChar>?
// CHECK-NEXT: }

// CHECK-NEXT: struct DerivedFromNonTrivial {
// CHECK-NEXT:   init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func inNonTrivial() -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func inNonTrivialWithArgs(_ a: Int32, _ b: Int32) -> UnsafePointer<CChar>?
// CHECK-NEXT: }

// CHECK-NEXT: struct PrivatelyInherited {
// CHECK-NEXT:   init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   mutating func mutatingInBase() -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func constInBase() -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func takesArgsInBase(_ a: Int32, _ b: Int32, _ c: Int32) -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func takesNonTrivialInBase(_ a: NonTrivial) -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func returnsNonTrivialInBase() -> NonTrivial
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   mutating func swiftRenamed(input i: Int32) -> Int32
// CHECK-NEXT:   @available(swift, obsoleted: 3, renamed: "swiftRenamed(input:)")
// CHECK-NEXT:   mutating func renamed(_ i: Int32) -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   @_effects(readonly) func pure() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func sameMethodNameSameSignature() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func sameMethodDifferentSignature() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: struct ProtectedInherited {
// CHECK-NEXT:   init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   mutating func mutatingInBase() -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func constInBase() -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func takesArgsInBase(_ a: Int32, _ b: Int32, _ c: Int32) -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func takesNonTrivialInBase(_ a: NonTrivial) -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func returnsNonTrivialInBase() -> NonTrivial
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   mutating func swiftRenamed(input i: Int32) -> Int32
// CHECK-NEXT:   @available(swift, obsoleted: 3, renamed: "swiftRenamed(input:)")
// CHECK-NEXT:   mutating func renamed(_ i: Int32) -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   @_effects(readonly) func pure() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func sameMethodNameSameSignature() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func sameMethodDifferentSignature() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: struct EmptyBaseClass {
// CHECK-NEXT:   init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func inBase() -> UnsafePointer<CChar>!
// CHECK-NEXT: }

// CHECK-NEXT: struct DerivedFromEmptyBaseClass {
// CHECK-NEXT:   init()
// CHECK-NEXT:   var a: Int32
// CHECK-NEXT:   var b: Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func inBase() -> UnsafePointer<CChar>?
// CHECK-NEXT: }

// CHECK-NEXT: @discardableResult
// CHECK-NEXT: func getCopyCounter() -> UnsafeMutablePointer<Int32>

// CHECK-NEXT: struct CopyTrackedBaseClass {
// CHECK-NEXT:   init(_ x: Int32)
// CHECK-NEXT:   @available(*, deprecated, message: "This zero-initializes the backing memory of the struct, which is unsafe for some C++ structs. Consider adding an explicit default initializer for this C++ struct.")
// CHECK-NEXT:   @_transparent init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func getX() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   mutating func getXMut() -> Int32
// CHECK-NEXT:   var x: Int32
// CHECK-NEXT: }

// CHECK-NEXT: struct CopyTrackedDerivedClass {
// CHECK-NEXT:   init(_ x: Int32)
// CHECK-NEXT:   @available(*, deprecated, message: "This zero-initializes the backing memory of the struct, which is unsafe for some C++ structs. Consider adding an explicit default initializer for this C++ struct.")
// CHECK-NEXT:   @_transparent init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func getDerivedX() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func getX() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   mutating func getXMut() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: struct NonEmptyBase {
// CHECK-NEXT:   init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func getY() -> Int32
// CHECK-NEXT:   var y: Int32
// CHECK-NEXT: }

// CHECK-NEXT: struct CopyTrackedDerivedDerivedClass {
// CHECK-NEXT:   init(_ x: Int32)
// CHECK-NEXT:   @available(*, deprecated, message: "This zero-initializes the backing memory of the struct, which is unsafe for some C++ structs. Consider adding an explicit default initializer for this C++ struct.")
// CHECK-NEXT:   @_transparent init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func getY() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func getDerivedX() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   func getX() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   mutating func getXMut() -> Int32
// CHECK-NEXT: }
