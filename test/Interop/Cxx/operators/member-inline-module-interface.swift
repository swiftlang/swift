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


// Non-reference subscript operators are not currently imported (SR-14351)
// so just make sure we don't crash.
// CHECK: struct NonReferenceReadIntArray {
// CHECK: }
