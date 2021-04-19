// RUN: %target-swift-ide-test -print-module -module-to-print=MemberInline -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: struct LoadableIntWrapper {
// CHECK:   static func - (lhs: inout LoadableIntWrapper, rhs: LoadableIntWrapper) -> LoadableIntWrapper
// CHECK:   mutating func callAsFunction() -> Int32
// CHECK:   mutating func callAsFunction(_ x: Int32) -> Int32
// CHECK:   mutating func callAsFunction(_ x: Int32, _ y: Int32) -> Int32
// CHECK: }

// CHECK: struct AddressOnlyIntWrapper {
// CHECK:   mutating func callAsFunction() -> Int32
// CHECK:   mutating func callAsFunction(_ x: Int32) -> Int32
// CHECK:   mutating func callAsFunction(_ x: Int32, _ y: Int32) -> Int32
// CHECK: }

// CHECK: struct HasDeletedOperator {
// CHECK: }


// CHECK: struct ReadWriteIntArray {
// CHECK:   struct NestedIntArray {
// CHECK:     @available(*, unavailable, message: "use subscript")
// CHECK:     mutating func __operatorSubscriptConst(_ x: Int32) -> UnsafePointer<Int32>

// CHECK:     subscript(x: Int32) -> Int32 { mutating get }
// CHECK:   }

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst(_ x: Int32) -> UnsafePointer<Int32>

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_ x: Int32) -> UnsafeMutablePointer<Int32>

// CHECK:   subscript(x: Int32) -> Int32 { mutating get set }
// CHECK: }


// CHECK: struct ReadOnlyIntArray {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst(_ x: Int32) -> UnsafePointer<Int32>

// CHECK:   subscript(x: Int32) -> Int32 { mutating get }
// CHECK: }


// CHECK: struct WriteOnlyIntArray {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_ x: Int32) -> UnsafeMutablePointer<Int32>

// CHECK:   subscript(x: Int32) -> Int32 { mutating get set }
// CHECK: }


// CHECK: struct DifferentTypesArray {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst(_ x: Int32) -> UnsafePointer<Int32>

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_ x: Int32) -> UnsafeMutablePointer<Int32>

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_ x: Bool) -> UnsafeMutablePointer<Bool>

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst(_ x: Bool) -> UnsafePointer<Bool>

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst(_ x: Double) -> UnsafePointer<Double>

// CHECK:   subscript(x: Int32) -> Int32 { mutating get set }
// CHECK:   subscript(x: Bool) -> Bool { mutating get set }
// CHECK:   subscript(x: Double) -> Double { mutating get }
// CHECK: }


// CHECK: struct TemplatedArray<T> {
// CHECK: }
// CHECK: struct __CxxTemplateInst14TemplatedArrayIdE {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_ i: Int32) -> UnsafeMutablePointer<Double>

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst(_ i: Int32) -> UnsafePointer<Double>

// CHECK:   subscript(i: Int32) -> Double { mutating get set }
// CHECK: }
// CHECK: typealias TemplatedDoubleArray = __CxxTemplateInst14TemplatedArrayIdE


// CHECK: struct TemplatedSubscriptArray {
// CHECK: }


// CHECK: struct IntArrayByVal {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst(_ x: Int32) -> Int32
// CHECK:   subscript(x: Int32) -> Int32 { mutating get }
// CHECK: }

// CHECK: struct NonTrivialIntArrayByVal {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst(_ x: Int32) -> Int32
// CHECK:   subscript(x: Int32) -> Int32 { mutating get }
// CHECK: }

// CHECK: struct DifferentTypesArrayByVal {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst(_ x: Int32) -> Int32

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst(_ x: Bool) -> Bool

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst(_ x: Double) -> Double

// CHECK:   subscript(x: Int32) -> Int32 { mutating get }
// CHECK:   subscript(x: Bool) -> Bool { mutating get }
// CHECK:   subscript(x: Double) -> Double { mutating get }
// CHECK: }


// CHECK: struct TemplatedArrayByVal<T> {
// CHECK: }
// CHECK: struct __CxxTemplateInst19TemplatedArrayByValIdE {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst(_ i: Int32) -> Double
// CHECK:   subscript(i: Int32) -> Double { mutating get }
// CHECK: }
// CHECK: typealias TemplatedDoubleArrayByVal = __CxxTemplateInst19TemplatedArrayByValIdE


// CHECK: struct TemplatedSubscriptArrayByVal {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst<T>(_ i: T) -> T
// CHECK:   subscript(i: T) -> T { mutating get }
// CHECK: }

// CHECK: struct NonTrivialArrayByVal {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst(_ x: Int32) -> NonTrivial
// CHECK:   subscript(x: Int32) -> NonTrivial { mutating get }
// CHECK: }
// CHECK: struct PtrByVal {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_ x: Int32) -> UnsafeMutablePointer<Int32>!
// CHECK:   subscript(x: Int32) -> UnsafeMutablePointer<Int32>! { mutating get }
// CHECK: }
// CHECK: struct RefToPtr {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_ x: Int32) -> UnsafeMutablePointer<UnsafeMutablePointer<Int32>?>
// CHECK:   subscript(x: Int32) -> UnsafeMutablePointer<Int32>? { mutating get set }
// CHECK: }
// CHECK: struct PtrToPtr {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_ x: Int32) -> UnsafeMutablePointer<UnsafeMutablePointer<Int32>?>!
// CHECK:   subscript(x: Int32) -> UnsafeMutablePointer<UnsafeMutablePointer<Int32>?>! { mutating get }
// CHECK: }
// CHECK: struct ConstOpPtrByVal {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst(_ x: Int32) -> UnsafePointer<Int32>!
// CHECK:   subscript(x: Int32) -> UnsafePointer<Int32>! { mutating get }
// CHECK: }
// CHECK: struct ConstPtrByVal {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst(_ x: Int32) -> UnsafePointer<Int32>!
// CHECK:   subscript(x: Int32) -> UnsafePointer<Int32>! { mutating get }
// CHECK: }