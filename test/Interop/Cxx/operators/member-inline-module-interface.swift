// RUN: %target-swift-ide-test -print-module -module-to-print=MemberInline -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default -Xcc -std=c++23 | %FileCheck %s

// CHECK: struct LoadableIntWrapper {
// CHECK:   mutating func callAsFunction() -> CInt
// CHECK:   mutating func callAsFunction(_ x: CInt) -> CInt
// CHECK:   mutating func callAsFunction(_ x: CInt, _ y: CInt) -> CInt
// CHECK:   static func - (lhs: inout LoadableIntWrapper, rhs: LoadableIntWrapper) -> LoadableIntWrapper
// CHECK:   static func += (lhs: inout LoadableIntWrapper, rhs: LoadableIntWrapper)
// CHECK:   func successor() -> LoadableIntWrapper
// CHECK: }
// CHECK: func == (lhs: LoadableIntWrapper, rhs: LoadableIntWrapper) -> CBool
// CHECK: func -= (lhs: inout LoadableIntWrapper, rhs: LoadableIntWrapper)

// CHECK: func == (lhs: NS.IntWrapperInNamespace, rhs: NS.IntWrapperInNamespace) -> CBool

// CHECK: struct LoadableBoolWrapper
// CHECK:   func __convertToBool() -> CBool
// CHECK:   prefix static func ! (lhs: inout LoadableBoolWrapper) -> LoadableBoolWrapper
// CHECK: }

// CHECK: struct AddressOnlyIntWrapper {
// CHECK:   mutating func callAsFunction() -> CInt
// CHECK:   mutating func callAsFunction(_ x: CInt) -> CInt
// CHECK:   mutating func callAsFunction(_ x: CInt, _ y: CInt) -> CInt
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

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   func __operatorSubscriptConst(_ x: CInt) -> UnsafePointer<CInt>

// CHECK:   subscript(x: CInt) -> CInt

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_ x: CInt) -> UnsafeMutablePointer<CInt>

// CHECK:      struct NestedIntArray {
// CHECK:        @available(*, unavailable, message: "use subscript")
// CHECK-NEXT:   func __operatorSubscriptConst(_ x: CInt) -> UnsafePointer<CInt>
// CHECK-NEXT:   subscript(x: CInt) -> CInt { get }
// CHECK:      }
// CHECK: }

// CHECK: struct NullarySubscript {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   func __operatorSubscriptConst() -> CInt
// CHECK:   subscript() -> CInt
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript() -> UnsafeMutablePointer<CInt>
// CHECK: }

// CHECK: struct BinarySubscript {
// CHECK:   init(field: CInt)
// CHECK:   init()
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   func __operatorSubscriptConst(_ x: CInt, _ y: CInt) -> CInt
// CHECK:   subscript(x: CInt, y: CInt) -> CInt
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_: CInt, _: CInt) -> UnsafeMutablePointer<CInt>
// CHECK:   var field: CInt
// CHECK: }

// CHECK: struct ReadOnlyIntArray {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   func __operatorSubscriptConst(_ x: CInt) -> UnsafePointer<CInt>
// CHECK:   subscript(x: CInt) -> CInt { get }
// CHECK: }


// CHECK: struct WriteOnlyIntArray {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_ x: CInt) -> UnsafeMutablePointer<CInt>
// CHECK:   subscript(x: CInt) -> CInt { mutating get set }
// CHECK: }


// CHECK: struct DifferentTypesArray {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   func __operatorSubscriptConst(_ x: CInt) -> UnsafePointer<CInt>
// CHECK:   subscript(x: CInt) -> CInt

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_ x: CInt) -> UnsafeMutablePointer<CInt>

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_ x: CBool) -> UnsafeMutablePointer<CBool>

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   func __operatorSubscriptConst(_ x: CBool) -> UnsafePointer<CBool>
// CHECK:   subscript(x: CBool) -> CBool

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   func __operatorSubscriptConst(_ x: CDouble) -> UnsafePointer<CDouble>
// CHECK:   subscript(x: CDouble) -> CDouble { get }

// CHECK: }


// CHECK: struct TemplatedArray<T> {
// CHECK: }
// CHECK: struct TemplatedArray<CDouble> {

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_ i: CInt) -> UnsafeMutablePointer<CDouble>

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   func __operatorSubscriptConst(_ i: CInt) -> UnsafePointer<CDouble>
// CHECK:   subscript(i: CInt) -> CDouble
// CHECK: }
// CHECK: typealias TemplatedDoubleArray = TemplatedArray<CDouble>


// CHECK: struct TemplatedSubscriptArray {
// CHECK: }


// CHECK: struct IntArrayByVal {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   func __operatorSubscriptConst(_ x: CInt) -> CInt
// CHECK:   subscript(x: CInt) -> CInt { get }
// CHECK: }

// CHECK: struct NonTrivialIntArrayByVal {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   func __operatorSubscriptConst(_ x: CInt) -> CInt
// CHECK:   subscript(x: CInt) -> CInt { get }
// CHECK: }

// CHECK: struct DifferentTypesArrayByVal {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst(_ x: CInt) -> CInt
// CHECK:   subscript(x: CInt) -> CInt { mutating get }

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst(_ x: CBool) -> CBool
// CHECK:   subscript(x: CBool) -> CBool { mutating get }

// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   func __operatorSubscriptConst(_ x: CDouble) -> CDouble
// CHECK:   subscript(x: CDouble) -> CDouble { get }
// CHECK: }


// CHECK: struct TemplatedArrayByVal<T> {
// CHECK: }
// CHECK: struct TemplatedArrayByVal<CDouble> {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst(_ i: CInt) -> CDouble
// CHECK:   subscript(i: CInt) -> CDouble { mutating get }
// CHECK: }
// CHECK: typealias TemplatedDoubleArrayByVal = TemplatedArrayByVal<CDouble>

// CHECK: struct TemplatedByVal<T> {
// CHECK-NEXT: }

// CHECK: struct TemplatedOperatorArrayByVal {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst<T>(_ i: T) -> T
// CHECK:   subscript<T>(i: T) -> T { mutating get }
// CHECK-NOT: mutating func __operatorPlus<T>(_ i: T) -> UnsafeMutablePointer<T>
// CHECK: }

// CHECK: struct NonTrivialArrayByVal {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst(_ x: CInt) -> NonTrivial
// CHECK:   subscript(x: CInt) -> NonTrivial { mutating get }
// CHECK: }
// CHECK: struct PtrByVal {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_ x: CInt) -> UnsafeMutablePointer<CInt>!
// CHECK:   subscript(x: CInt) -> UnsafeMutablePointer<CInt>! { mutating get set }
// CHECK: }
// CHECK: struct RefToPtr {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_ x: CInt) -> UnsafeMutablePointer<UnsafeMutablePointer<CInt>?>
// CHECK:   subscript(x: CInt) -> UnsafeMutablePointer<CInt>? { mutating get set }
// CHECK: }
// CHECK: struct PtrToPtr {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscript(_ x: CInt) -> UnsafeMutablePointer<UnsafeMutablePointer<CInt>?>!
// CHECK:   subscript(x: CInt) -> UnsafeMutablePointer<UnsafeMutablePointer<CInt>?>! { mutating get set }
// CHECK: }
// CHECK: struct ConstOpPtrByVal {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   func __operatorSubscriptConst(_ x: CInt) -> UnsafePointer<CInt>!
// CHECK:   subscript(x: CInt) -> UnsafePointer<CInt>! { get }
// CHECK: }
// CHECK: struct ConstPtrByVal {
// CHECK:   @available(*, unavailable, message: "use subscript")
// CHECK:   mutating func __operatorSubscriptConst(_ x: CInt) -> UnsafePointer<CInt>!
// CHECK:   subscript(x: CInt) -> UnsafePointer<CInt>! { mutating get }
// CHECK: }

// CHECK: struct DerivedFromAddressOnlyIntWrapper {
// CHECK:   mutating func callAsFunction() -> CInt
// CHECK:   mutating func callAsFunction(_ x: CInt) -> CInt
// CHECK:   mutating func callAsFunction(_ x: CInt, _ y: CInt) -> CInt
// CHECK: }

// CHECK: struct DerivedFromReadWriteIntArray {
// CHECK:   func __operatorSubscriptConst(_ x: CInt) -> UnsafePointer<CInt>
// CHECK:   mutating func __operatorSubscript(_ x: CInt) -> UnsafeMutablePointer<CInt>
// CHECK:   subscript(x: CInt) -> CInt
// CHECK: }

// CHECK: struct DerivedFromNonTrivialArrayByVal {
// CHECK:   mutating func __operatorSubscriptConst(_ x: CInt) -> NonTrivial
// CHECK:   subscript(x: CInt) -> NonTrivial { mutating get }
// CHECK: }

// CHECK: struct SubscriptUnnamedParameter {
// CHECK:   subscript(__index0: CInt) -> CInt { get }
// CHECK: }
// CHECK: struct SubscriptUnnamedParameterReadWrite {
// CHECK:   subscript(__index0: CInt) -> CInt
// CHECK: }

// CHECK: struct Iterator {
// CHECK:   @available(*, unavailable, message: "use .pointee property")
// CHECK:   mutating func __operatorStar() -> UnsafeMutablePointer<CInt>
// CHECK:   var pointee: CInt { mutating get set }
// CHECK: }

// CHECK: struct ConstIterator {
// CHECK:   @available(*, unavailable, message: "use .pointee property")
// CHECK:   func __operatorStar() -> UnsafePointer<CInt>
// CHECK:   var pointee: CInt { get }
// CHECK: }

// CHECK: struct ConstIteratorByVal {
// CHECK:   @available(*, unavailable, message: "use .pointee property")
// CHECK:   func __operatorStar() -> CInt
// CHECK:   var pointee: CInt { get }
// CHECK: }

// CHECK: struct AllStar {
// CHECK-NEXT:   init()
// CHECK-NEXT:   var L: CLong{{(32|64)?}}
// CHECK-NEXT:   @available(*, unavailable, message: "use .pointee property")
// CHECK-NEXT:   mutating func __operatorStar() -> UnsafeMutablePointer<CLong{{(32|64)?}}>
// CHECK-NEXT:   @available(*, unavailable, message: "use * instead")
// CHECK-NEXT:   func __operatorStar(_ rhs: AllStar) -> AllStar
// CHECK-NEXT:   static func * (lhs: AllStar, rhs: AllStar) -> AllStar
// CHECK-NEXT:   var pointee: CLong{{(32|64)?}} { mutating get set }
// CHECK-NEXT: }

// CHECK: struct AmbiguousOperatorStar {
// CHECK-NEXT:   init()
// CHECK-NEXT:   var value: CInt
// CHECK-NEXT:   @available(*, unavailable, message: "use .pointee property")
// CHECK-NEXT:   mutating func __operatorStar() -> UnsafeMutablePointer<CInt>
// CHECK-NEXT:   @available(*, unavailable, message: "use .pointee property")
// CHECK-NEXT:   func __operatorStar() -> UnsafePointer<CInt>
// CHECK-NEXT:   var pointee: CInt
// CHECK-NEXT: }

// CHECK: struct AmbiguousOperatorStar2 {
// CHECK-NEXT:   init()
// CHECK-NEXT:   var value: CInt
// CHECK-NEXT:   @available(*, unavailable, message: "use .pointee property")
// CHECK-NEXT:   mutating func __operatorStar() -> UnsafeMutablePointer<CInt>
// CHECK-NEXT:   @available(*, unavailable, message: "use .pointee property")
// CHECK-NEXT:   func __operatorStar() -> UnsafePointer<CInt>
// CHECK-NEXT:   var pointee: CInt
// CHECK-NEXT: }

// CHECK: struct DerivedFromConstIterator {
// CHECK-NEXT:   init()
// CHECK-NEXT:   @available(*, unavailable, message: "use .pointee property")
// CHECK-NEXT:   func __operatorStar() -> UnsafePointer<CInt>
// CHECK-NEXT:   var pointee: CInt { get }
// CHECK-NEXT: }

// CHECK: struct DerivedFromConstIteratorPrivatelyWithUsingDecl {
// CHECK-NEXT:   init()
// CHECK-NEXT:   @available(*, unavailable, message: "use .pointee property")
// CHECK-NEXT:   func __operatorStar() -> UnsafePointer<CInt>
// CHECK-NEXT:   var pointee: CInt { get }
// CHECK: }

// CHECK: struct DerivedFromAmbiguousOperatorStarPrivatelyWithUsingDecl {
// CHECK-NEXT:   init()
// CHECK-NEXT:   @available(*, unavailable, message: "use .pointee property")
// CHECK-NEXT:   mutating func __operatorStar() -> UnsafeMutablePointer<CInt>
// CHECK-NEXT:   @available(*, unavailable, message: "use .pointee property")
// CHECK-NEXT:   func __operatorStar() -> UnsafePointer<CInt>
// CHECK-NEXT:   var pointee: CInt
// CHECK: }

// CHECK: struct DerivedFromLoadableIntWrapperWithUsingDecl {
// CHECK-NEXT:   init()
// CHECK-NEXT:   @available(*, unavailable, message: "use - instead")
// CHECK-NEXT:   mutating func __operatorMinus(_ rhs: LoadableIntWrapper) -> LoadableIntWrapper
// CHECK-NEXT:   @available(*, unavailable, message: "use += instead")
// CHECK-NEXT:   mutating func __operatorPlusEqual(_ rhs: LoadableIntWrapper) -> LoadableIntWrapper
// CHECK-NEXT:   func getValue() -> CInt
// CHECK-NEXT:   mutating func setValue(_ v: CInt)
// CHECK-NEXT:   static func - (lhs: inout DerivedFromLoadableIntWrapperWithUsingDecl, rhs: LoadableIntWrapper) -> LoadableIntWrapper
// CHECK-NEXT:   static func += (lhs: inout DerivedFromLoadableIntWrapperWithUsingDecl, rhs: LoadableIntWrapper) -> LoadableIntWrapper
// CHECK-NEXT:   func successor() -> DerivedFromLoadableIntWrapperWithUsingDecl
// CHECK:      }

// CHECK: struct HasOperatorCallWithDefaultArg {
// CHECK:   func callAsFunction(_ x: CInt = cxxDefaultArg) -> CInt
// CHECK: }

// CHECK: struct HasStaticOperatorCallBase {
// CHECK:   func callAsFunction(_ x: CInt) -> CInt
// CHECK: }

// CHECK: struct HasStaticOperatorCallDerived {
// CHECK:   func callAsFunction(_ x: CInt) -> CInt
// CHECK: }

// CHECK: struct HasStaticOperatorCallWithConstOperator {
// CHECK:   func callAsFunction(_ x: CInt) -> CInt
// CHECK:   func callAsFunction(_ x: CInt, _ y: CInt) -> CInt
// CHECK: }

// CHECK: struct HasStaticOperatorCallWithUnimportableCxxType {
// CHECK-NEXT:  init()
// CHECK-NEXT: }

// CHECK: struct HasOperatorReturningAuto {
// CHECK-NEXT:  init()
// CHECK-NEXT: }

