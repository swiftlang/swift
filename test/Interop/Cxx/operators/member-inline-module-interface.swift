// RUN: %target-swift-ide-test -print-module -module-to-print=MemberInline -I %S/Inputs -source-filename=x -cxx-interoperability-mode=swift-5.9 | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=MemberInline -I %S/Inputs -source-filename=x -cxx-interoperability-mode=swift-6 | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=MemberInline -I %S/Inputs -source-filename=x -cxx-interoperability-mode=upcoming-swift | %FileCheck %s

// CHECK: struct LoadableIntWrapper {
// CHECK:   func successor() -> LoadableIntWrapper
// CHECK:   static func - (lhs: inout LoadableIntWrapper, rhs: LoadableIntWrapper) -> LoadableIntWrapper
// CHECK:   static func += (lhs: inout LoadableIntWrapper, rhs: LoadableIntWrapper)
// CHECK:   mutating func callAsFunction() -> Int32
// CHECK:   mutating func callAsFunction(_ x: Int32) -> Int32
// CHECK:   mutating func callAsFunction(_ x: Int32, _ y: Int32) -> Int32
// CHECK: }
// CHECK: func == (lhs: LoadableIntWrapper, rhs: LoadableIntWrapper) -> Bool
// CHECK: func -= (lhs: inout LoadableIntWrapper, rhs: LoadableIntWrapper)

// CHECK: func == (lhs: NS.IntWrapperInNamespace, rhs: NS.IntWrapperInNamespace) -> Bool

// CHECK: struct LoadableBoolWrapper
// CHECK:   prefix static func ! (lhs: inout LoadableBoolWrapper) -> LoadableBoolWrapper
// CHECK:   func __convertToBool() -> Bool
// CHECK: }

// CHECK: struct AddressOnlyIntWrapper {
// CHECK:   mutating func callAsFunction() -> Int32
// CHECK:   mutating func callAsFunction(_ x: Int32) -> Int32
// CHECK:   mutating func callAsFunction(_ x: Int32, _ y: Int32) -> Int32
// CHECK: }

// CHECK: struct HasPostIncrementOperator {
// CHECK: }

// CHECK: struct HasPreIncrementOperatorWithAnotherReturnType {
// CHECK:   func successor() -> HasPreIncrementOperatorWithAnotherReturnType
// CHECK: }

// CHECK: struct HasPreIncrementOperatorWithVoidReturnType {
// CHECK:   func successor() -> HasPreIncrementOperatorWithVoidReturnType
// CHECK: }

// CHECK: struct HasDeletedOperator {
// CHECK: }


// CHECK: struct ReadWriteIntArray {
// CHECK:   subscript(x: Int32) -> Int32

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   func __operatorSubscriptConst(_ x: Int32) -> UnsafePointer<Int32>

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_ x: Int32) -> UnsafeMutablePointer<Int32>

// CHECK:   struct NestedIntArray {
// CHECK:     subscript(x: Int32) -> Int32 { get }

// CHECK:     @available(*, unavailable, message: "use subscript")
// CHECK:     func __operatorSubscriptConst(_ x: Int32) -> UnsafePointer<Int32>
// CHECK:   }
// CHECK: }


// CHECK: struct ReadOnlyIntArray {
// CHECK:   subscript(x: Int32) -> Int32 { get }

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   func __operatorSubscriptConst(_ x: Int32) -> UnsafePointer<Int32>
// CHECK: }


// CHECK: struct WriteOnlyIntArray {
// CHECK:   subscript(x: Int32) -> Int32 { mutating get set }

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_ x: Int32) -> UnsafeMutablePointer<Int32>
// CHECK: }


// CHECK: struct DifferentTypesArray {
// CHECK:   subscript(x: Int32) -> Int32
// CHECK:   subscript(x: Bool) -> Bool
// CHECK:   subscript(x: Double) -> Double { get }

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   func __operatorSubscriptConst(_ x: Int32) -> UnsafePointer<Int32>

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_ x: Int32) -> UnsafeMutablePointer<Int32>

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_ x: Bool) -> UnsafeMutablePointer<Bool>

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   func __operatorSubscriptConst(_ x: Bool) -> UnsafePointer<Bool>

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   func __operatorSubscriptConst(_ x: Double) -> UnsafePointer<Double>

// CHECK: }


// CHECK: struct TemplatedArray<T> {
// CHECK: }
// CHECK: struct TemplatedArray<CDouble> {
// CHECK:   subscript(i: Int32) -> Double

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_ i: Int32) -> UnsafeMutablePointer<Double>

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   func __operatorSubscriptConst(_ i: Int32) -> UnsafePointer<Double>
// CHECK: }
// CHECK: typealias TemplatedDoubleArray = TemplatedArray<CDouble>


// CHECK: struct TemplatedSubscriptArray {
// CHECK: }


// CHECK: struct IntArrayByVal {
// CHECK:   subscript(x: Int32) -> Int32 { get }
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   func __operatorSubscriptConst(_ x: Int32) -> Int32
// CHECK: }

// CHECK: struct NonTrivialIntArrayByVal {
// CHECK:   subscript(x: Int32) -> Int32 { get }
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   func __operatorSubscriptConst(_ x: Int32) -> Int32
// CHECK: }

// CHECK: struct DifferentTypesArrayByVal {
// CHECK:   subscript(x: Int32) -> Int32 { mutating get }
// CHECK:   subscript(x: Bool) -> Bool { mutating get }
// CHECK:   subscript(x: Double) -> Double { get }

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   func __operatorSubscriptConst(_ x: Int32) -> Int32

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   func __operatorSubscriptConst(_ x: Bool) -> Bool

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   func __operatorSubscriptConst(_ x: Double) -> Double
// CHECK: }


// CHECK: struct TemplatedArrayByVal<T> {
// CHECK: }
// CHECK: struct TemplatedArrayByVal<CDouble> {
// CHECK:   subscript(i: Int32) -> Double { mutating get }
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst(_ i: Int32) -> Double
// CHECK: }
// CHECK: typealias TemplatedDoubleArrayByVal = TemplatedArrayByVal<CDouble>

// CHECK: struct TemplatedByVal<T> {
// CHECK-NEXT: }

// CHECK: struct TemplatedOperatorArrayByVal {
// CHECK:   subscript<T>(i: T) -> T { mutating get }
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst<T>(_ i: T) -> T
// CHECK-NOT: mutating func __operatorPlus<T>(_ i: T) -> UnsafeMutablePointer<T>
// CHECK: }

// CHECK: struct NonTrivialArrayByVal {
// CHECK:   subscript(x: Int32) -> NonTrivial { mutating get }
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst(_ x: Int32) -> NonTrivial
// CHECK: }
// CHECK: struct PtrByVal {
// CHECK:   subscript(x: Int32) -> UnsafeMutablePointer<Int32>! { mutating get }
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_ x: Int32) -> UnsafeMutablePointer<Int32>!
// CHECK: }
// CHECK: struct RefToPtr {
// CHECK:   subscript(x: Int32) -> UnsafeMutablePointer<Int32>? { mutating get set }
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_ x: Int32) -> UnsafeMutablePointer<UnsafeMutablePointer<Int32>?>
// CHECK: }
// CHECK: struct PtrToPtr {
// CHECK:   subscript(x: Int32) -> UnsafeMutablePointer<UnsafeMutablePointer<Int32>?>! { mutating get }
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_ x: Int32) -> UnsafeMutablePointer<UnsafeMutablePointer<Int32>?>!
// CHECK: }
// CHECK: struct ConstOpPtrByVal {
// CHECK:   subscript(x: Int32) -> UnsafePointer<Int32>! { get }
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   func __operatorSubscriptConst(_ x: Int32) -> UnsafePointer<Int32>!
// CHECK: }
// CHECK: struct ConstPtrByVal {
// CHECK:   subscript(x: Int32) -> UnsafePointer<Int32>! { mutating get }
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst(_ x: Int32) -> UnsafePointer<Int32>!
// CHECK: }

// CHECK: struct DerivedFromAddressOnlyIntWrapper {
// CHECK:   mutating func callAsFunction() -> Int32
// CHECK:   mutating func callAsFunction(_ x: Int32) -> Int32
// CHECK:   mutating func callAsFunction(_ x: Int32, _ y: Int32) -> Int32
// CHECK: }

// CHECK: struct DerivedFromReadWriteIntArray {
// CHECK:   subscript(x: Int32) -> Int32
// CHECK:   func __operatorSubscriptConst(_ x: Int32) -> UnsafePointer<Int32>
// CHECK:   mutating func __operatorSubscript(_ x: Int32) -> UnsafeMutablePointer<Int32>
// CHECK: }

// CHECK: struct DerivedFromNonTrivialArrayByVal {
// CHECK:   subscript(x: Int32) -> NonTrivial { get }
// CHECK:   mutating func __operatorSubscriptConst(_ x: Int32) -> NonTrivial
// CHECK: }

// CHECK: struct SubscriptUnnamedParameter {
// CHECK:   subscript(__index: Int32) -> Int32 { get }
// CHECK: }
// CHECK: struct SubscriptUnnamedParameterReadWrite {
// CHECK:   subscript(__index: Int32) -> Int32
// CHECK: }

// CHECK: struct Iterator {
// CHECK:   var pointee: Int32 { mutating get set }
// CHECK:   @available(*, unavailable, message: "use .pointee property")
// CHECK:   mutating func __operatorStar() -> UnsafeMutablePointer<Int32>
// CHECK: }

// CHECK: struct ConstIterator {
// CHECK:   var pointee: Int32 { get }
// CHECK:   @available(*, unavailable, message: "use .pointee property")
// CHECK:   func __operatorStar() -> UnsafePointer<Int32>
// CHECK: }

// CHECK: struct ConstIteratorByVal {
// CHECK:   var pointee: Int32 { get }
// CHECK:   @available(*, unavailable, message: "use .pointee property")
// CHECK:   func __operatorStar() -> Int32
// CHECK: }

// CHECK: struct AmbiguousOperatorStar {
// CHECK-NEXT:   init()
// CHECK-NEXT:   var pointee: Int32
// CHECK-NEXT:   @available(*, unavailable, message: "use .pointee property")
// CHECK-NEXT:   mutating func __operatorStar() -> UnsafeMutablePointer<Int32>
// CHECK-NEXT:   @available(*, unavailable, message: "use .pointee property")
// CHECK-NEXT:   func __operatorStar() -> UnsafePointer<Int32>
// CHECK-NEXT:   var value: Int32
// CHECK-NEXT: }

// CHECK: struct AmbiguousOperatorStar2 {
// CHECK-NEXT:   init()
// CHECK-NEXT:   var pointee: Int32
// CHECK-NEXT:   @available(*, unavailable, message: "use .pointee property")
// CHECK-NEXT:   mutating func __operatorStar() -> UnsafeMutablePointer<Int32>
// CHECK-NEXT:   @available(*, unavailable, message: "use .pointee property")
// CHECK-NEXT:   func __operatorStar() -> UnsafePointer<Int32>
// CHECK-NEXT:   @available(*, unavailable, message: "use .pointee property")
// CHECK-NEXT:   func __operatorStar() -> UnsafePointer<Int32>
// CHECK-NEXT:   var value: Int32
// CHECK-NEXT: }

// CHECK: struct DerivedFromConstIterator {
// CHECK-NEXT:   init()
// TODO:   @available(*, unavailable, message: "use .pointee property")
// CHECK-NEXT:   func __operatorStar() -> UnsafePointer<Int32>
// TODO: `var pointee` should be printed here
// CHECK-NEXT: }

// CHECK: struct DerivedFromConstIteratorPrivatelyWithUsingDecl {
// CHECK-NEXT:   init()
// CHECK-NEXT:   var pointee: Int32 { get }
// CHECK-NEXT:   @available(*, unavailable, message: "use .pointee property")
// CHECK-NEXT:   func __operatorStar() -> UnsafePointer<Int32>
// CHECK-NEXT: }

// CHECK: struct DerivedFromAmbiguousOperatorStarPrivatelyWithUsingDecl {
// CHECK-NEXT:   init()
// CHECK-NEXT:   var pointee: Int32
// CHECK-NEXT:   @available(*, unavailable, message: "use .pointee property")
// CHECK-NEXT:   mutating func __operatorStar() -> UnsafeMutablePointer<Int32>
// CHECK-NEXT:   @available(*, unavailable, message: "use .pointee property")
// CHECK-NEXT:   func __operatorStar() -> UnsafePointer<Int32>
// CHECK-NEXT: }

// CHECK: struct DerivedFromLoadableIntWrapperWithUsingDecl {
// CHECK-NEXT:   init()
// CHECK-NEXT:   static func - (lhs: inout DerivedFromLoadableIntWrapperWithUsingDecl, rhs: LoadableIntWrapper) -> LoadableIntWrapper
// CHECK-NEXT:   @available(*, unavailable, message: "use - instead")
// CHECK-NEXT:   mutating func __operatorMinus(_ rhs: LoadableIntWrapper) -> LoadableIntWrapper
// CHECK-NEXT:   static func += (lhs: inout DerivedFromLoadableIntWrapperWithUsingDecl, rhs: LoadableIntWrapper)
// CHECK-NEXT:   @available(*, unavailable, message: "use += instead")
// CHECK-NEXT:   mutating func __operatorPlusEqual(_ rhs: LoadableIntWrapper)
// CHECK-NEXT:   func getValue() -> Int32
// CHECK-NEXT:   mutating func setValue(_ v: Int32)
// CHECK-NEXT: }

// CHECK: struct HasOperatorCallWithDefaultArg {
// CHECK:   func callAsFunction(_ x: Int32 = cxxDefaultArg) -> Int32
// CHECK: }

// CHECK: struct HasStaticOperatorCallBase {
// CHECK:   func callAsFunction(_ x: Int32) -> Int32
// CHECK: }

// CHECK: struct HasStaticOperatorCallDerived {
// CHECK:   func callAsFunction(_ x: Int32) -> Int32
// CHECK: }

// CHECK: struct HasStaticOperatorCallWithConstOperator {
// CHECK:   func callAsFunction(_ x: Int32, _ y: Int32) -> Int32
// CHECK:   func callAsFunction(_ x: Int32) -> Int32
// CHECK: }

// CHECK: struct HasStaticOperatorCallWithUnimportableCxxType {
// CHECK-NEXT:  init()
// CHECK-NEXT: }
