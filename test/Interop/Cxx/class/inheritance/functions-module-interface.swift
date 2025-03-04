// RUN: %target-swift-ide-test -print-module -print-implicit-attrs -print-access -module-to-print=Functions -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK:      public struct NonTrivial {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func inNonTrivial() -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func inNonTrivialWithArgs(_ a: Int32, _ b: Int32) -> UnsafePointer<CChar>!
// CHECK-NEXT: }

// CHECK-NEXT: public struct Base {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public mutating func mutatingInBase() -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func constInBase() -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func rvalueThisInBase() -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func takesArgsInBase(_ a: Int32, _ b: Int32, _ c: Int32) -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func takesNonTrivialInBase(_ a: NonTrivial) -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func returnsNonTrivialInBase() -> NonTrivial
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func templateInBase<T>(_ t: T) -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public static func staticInBase() -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public mutating func swiftRenamed(input i: Int32) -> Int32
// CHECK-NEXT:   @available(swift, obsoleted: 3, renamed: "swiftRenamed(input:)")
// CHECK-NEXT:   public mutating func renamed(_ i: Int32) -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   @_effects(readonly) public func pure() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func sameMethodNameSameSignature() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func sameMethodDifferentSignature() -> Int32
// CHECK-NEXT: }

// CHECK-NEXT: public struct OtherBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func inOtherBase() -> UnsafePointer<CChar>!
// CHECK-NEXT: }

// CHECK-NEXT: public struct Derived {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func inDerived() -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func sameMethodNameSameSignature() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func sameMethodDifferentSignature(_ x: Int32) -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public mutating func mutatingInBase() -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func constInBase() -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func takesArgsInBase(_ a: Int32, _ b: Int32, _ c: Int32) -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func takesNonTrivialInBase(_ a: NonTrivial) -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func returnsNonTrivialInBase() -> NonTrivial
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public mutating func swiftRenamed(input i: Int32) -> Int32
// CHECK-NEXT:   @available(swift, obsoleted: 3, renamed: "swiftRenamed(input:)")
// CHECK-NEXT:   public mutating func renamed(_ i: Int32) -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   @_effects(readonly) public func pure() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func sameMethodDifferentSignature() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func inOtherBase() -> UnsafePointer<CChar>?
// CHECK-NEXT: }

// CHECK-NEXT: public struct DerivedFromDerived {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func topLevel() -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func inDerived() -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func sameMethodNameSameSignature() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func sameMethodDifferentSignature(_ x: Int32) -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public mutating func mutatingInBase() -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func constInBase() -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func takesArgsInBase(_ a: Int32, _ b: Int32, _ c: Int32) -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func takesNonTrivialInBase(_ a: NonTrivial) -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func returnsNonTrivialInBase() -> NonTrivial
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public mutating func swiftRenamed(input i: Int32) -> Int32
// CHECK-NEXT:   @available(swift, obsoleted: 3, renamed: "swiftRenamed(input:)")
// CHECK-NEXT:   public mutating func renamed(_ i: Int32) -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   @_effects(readonly) public func pure() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func sameMethodDifferentSignature() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func inOtherBase() -> UnsafePointer<CChar>?
// CHECK-NEXT: }

// CHECK-NEXT: public struct DerivedFromNonTrivial {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func inNonTrivial() -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func inNonTrivialWithArgs(_ a: Int32, _ b: Int32) -> UnsafePointer<CChar>?
// CHECK-NEXT: }

// CHECK-NEXT: public struct PrivatelyInherited {
// CHECK-NEXT:   public init()
// CHECK-NOT:    public
// CHECK:      }

// CHECK-NEXT: public struct ProtectedInherited {
// CHECK-NEXT:   public init()
// CHECK-NOT:    public
// CHECK:      }

// CHECK-NEXT: public struct EmptyBaseClass {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func inBase() -> UnsafePointer<CChar>!
// CHECK-NEXT: }

// CHECK-NEXT: public struct DerivedFromEmptyBaseClass {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   public var a: Int32
// CHECK-NEXT:   public var b: Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func inBase() -> UnsafePointer<CChar>?
// CHECK-NEXT: }

// CHECK-NEXT: @discardableResult
// CHECK-NEXT: public func getCopyCounter() -> UnsafeMutablePointer<Int32>
// CHECK-NEXT: public struct CopyTrackedBaseClass {
// CHECK-NEXT:   public init(_ x: Int32)
// CHECK-NEXT:   @available(*, deprecated, message: "This zero-initializes the backing memory of the struct, which is unsafe for some C++ structs. Consider adding an explicit default initializer for this C++ struct.")
// CHECK-NEXT:   @_transparent public init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func getX() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public mutating func getXMut() -> Int32
// CHECK-NOT:    public
// CHECK:      }

// CHECK-NEXT: public struct CopyTrackedDerivedClass {
// CHECK-NEXT:   public init(_ x: Int32)
// CHECK-NEXT:   @available(*, deprecated, message: "This zero-initializes the backing memory of the struct, which is unsafe for some C++ structs. Consider adding an explicit default initializer for this C++ struct.")
// CHECK-NEXT:   @_transparent public init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func getDerivedX() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func getX() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public mutating func getXMut() -> Int32
// CHECK-NOT:    public
// CHECK:      }

// CHECK-NEXT: public struct NonEmptyBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func getY() -> Int32
// CHECK-NOT:    public
// CHECK:      }

// CHECK-NEXT: public struct CopyTrackedDerivedDerivedClass {
// CHECK-NEXT:   public init(_ x: Int32)
// CHECK-NEXT:   @available(*, deprecated, message: "This zero-initializes the backing memory of the struct, which is unsafe for some C++ structs. Consider adding an explicit default initializer for this C++ struct.")
// CHECK-NEXT:   @_transparent public init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func getY() -> Int32
// CHECK-NOT:    public
// CHECK:        @discardableResult
// CHECK-NEXT:   public func getDerivedX() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func getX() -> Int32
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public mutating func getXMut() -> Int32
// CHECK-NOT:    public
// CHECK:      }
