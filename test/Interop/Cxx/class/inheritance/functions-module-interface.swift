// RUN: %target-swift-ide-test -print-module -print-implicit-attrs -print-access -module-to-print=Functions -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK:      public struct NonTrivial {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func inNonTrivial() -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func inNonTrivialWithArgs(_ a: CInt, _ b: CInt) -> UnsafePointer<CChar>!
// CHECK-NEXT: }

// CHECK-NEXT: public struct Base {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public mutating func mutatingInBase() -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func constInBase() -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public consuming func rvalueThisInBase() -> UnsafePointer<CChar>!
// CHECK-NEXT:   public mutating func refQualifierOverloadsMutating()
// CHECK-NEXT:   public func refQualifierOverloads()
// CHECK-NEXT:   public consuming func refQualifierOverloadsMutatingConsuming()
// CHECK-NEXT:   public consuming func refQualifierOverloadsConsuming()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func takesArgsInBase(_ a: CInt, _ b: CInt, _ c: CInt) -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func takesNonTrivialInBase(_ a: NonTrivial) -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func returnsNonTrivialInBase() -> NonTrivial
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func templateInBase<T>(_ t: T) -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public static func staticInBase() -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public mutating func swiftRenamed(input i: CInt) -> CInt
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   @_effects(readonly) public func pure() -> CInt
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func sameMethodNameSameSignature() -> CInt
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func sameMethodDifferentSignature() -> CInt
// CHECK-NEXT: }

// CHECK-NEXT: public struct OtherBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func inOtherBase() -> UnsafePointer<CChar>!
// CHECK-NEXT: }

// CHECK-NEXT: public struct Derived {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public mutating func mutatingInBase() -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func constInBase() -> UnsafePointer<CChar>?
// CHECK-NEXT:   public mutating func refQualifierOverloadsMutating()
// CHECK-NEXT:   public func refQualifierOverloads()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func takesArgsInBase(_ a: CInt, _ b: CInt, _ c: CInt) -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func takesNonTrivialInBase(_ a: NonTrivial) -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func returnsNonTrivialInBase() -> NonTrivial
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public mutating func swiftRenamed(input i: CInt) -> CInt
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   @_effects(readonly) public func pure() -> CInt
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func sameMethodDifferentSignature() -> CInt
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func inOtherBase() -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func inDerived() -> UnsafePointer<CChar>!
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func sameMethodNameSameSignature() -> CInt
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func sameMethodDifferentSignature(_ x: CInt) -> CInt
// CHECK-NEXT: }

// CHECK-NEXT: public struct DerivedFromDerived {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public mutating func mutatingInBase() -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func constInBase() -> UnsafePointer<CChar>?
// CHECK-NEXT:   public mutating func refQualifierOverloadsMutating()
// CHECK-NEXT:   public func refQualifierOverloads()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func takesArgsInBase(_ a: CInt, _ b: CInt, _ c: CInt) -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func takesNonTrivialInBase(_ a: NonTrivial) -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func returnsNonTrivialInBase() -> NonTrivial
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public mutating func swiftRenamed(input i: CInt) -> CInt
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   @_effects(readonly) public func pure() -> CInt
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func sameMethodDifferentSignature() -> CInt
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func inOtherBase() -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func inDerived() -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func sameMethodNameSameSignature() -> CInt
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func sameMethodDifferentSignature(_ x: CInt) -> CInt
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func topLevel() -> UnsafePointer<CChar>!
// CHECK-NEXT: }

// CHECK-NEXT: public struct DerivedFromNonTrivial {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func inNonTrivial() -> UnsafePointer<CChar>?
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func inNonTrivialWithArgs(_ a: CInt, _ b: CInt) -> UnsafePointer<CChar>?
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
// CHECK-NEXT:   public init(a: CInt, b: CInt)
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func inBase() -> UnsafePointer<CChar>?
// CHECK-NEXT:   public var a: CInt
// CHECK-NEXT:   public var b: CInt
// CHECK-NEXT: }

// CHECK-NEXT: @discardableResult
// CHECK-NEXT: public func getCopyCounter() -> UnsafeMutablePointer<CInt>
// CHECK-NEXT: public struct CopyTrackedBaseClass {
// CHECK-NEXT:   @available(*, deprecated, message: "This zero-initializes the backing memory of the struct, which is unsafe for some C++ structs. Consider adding an explicit default initializer for this C++ struct.")
// CHECK-NEXT:   @_transparent public init()
// CHECK-NEXT:   public init(_ x: CInt)
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func getX() -> CInt
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public mutating func getXMut() -> CInt
// CHECK-NOT:    public
// CHECK:      }

// CHECK-NEXT: public struct CopyTrackedDerivedClass {
// CHECK-NEXT:   @available(*, deprecated, message: "This zero-initializes the backing memory of the struct, which is unsafe for some C++ structs. Consider adding an explicit default initializer for this C++ struct.")
// CHECK-NEXT:   @_transparent public init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func getX() -> CInt
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public mutating func getXMut() -> CInt
// CHECK-NEXT:   public init(_ x: CInt)
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func getDerivedX() -> CInt
// CHECK-NOT:    public
// CHECK:      }

// CHECK-NEXT: public struct NonEmptyBase {
// CHECK-NEXT:   public init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func getY() -> CInt
// CHECK-NOT:    public
// CHECK:      }

// CHECK-NEXT: public struct CopyTrackedDerivedDerivedClass {
// CHECK-NEXT:   @available(*, deprecated, message: "This zero-initializes the backing memory of the struct, which is unsafe for some C++ structs. Consider adding an explicit default initializer for this C++ struct.")
// CHECK-NEXT:   @_transparent public init()
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func getX() -> CInt
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public mutating func getXMut() -> CInt
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func getDerivedX() -> CInt
// CHECK-NEXT:   @discardableResult
// CHECK-NEXT:   public func getY() -> CInt
// CHECK-NEXT:   public init(_ x: CInt)
// CHECK-NOT:    public
// CHECK:      }
