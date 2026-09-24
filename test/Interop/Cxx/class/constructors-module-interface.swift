// RUN: %target-swift-ide-test -print-module -module-to-print=Constructors -I %S/Inputs/ -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s
// XFAIL: OS=linux-androideabi

// CHECK:      struct ExplicitDefaultConstructor {
// CHECK-NEXT:   init()
// CHECK-NEXT:   var x: CInt
// CHECK-NEXT: }
// CHECK-NEXT: struct ImplicitDefaultConstructor {
// CHECK-NEXT:   init(x: CInt)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var x: CInt
// CHECK-NEXT: }
// CHECK-NEXT: struct DefaultedDefaultConstructor {
// CHECK-NEXT:   init(x: CInt)
// CHECK-NEXT:   var x: CInt
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct MemberOfClassType {
// CHECK-NEXT:   init(member: ImplicitDefaultConstructor)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var member: ImplicitDefaultConstructor
// CHECK-NEXT: }
// CHECK-NEXT: struct DefaultConstructorDeleted {
// CHECK-NEXT:   init(a: UnsafeMutablePointer<CInt>)
// CHECK-NEXT:   var a: UnsafeMutablePointer<CInt>
// CHECK-NEXT: }
// CHECK-NEXT: struct ConstructorWithParam {
// CHECK-NEXT:   @available(*, deprecated, message
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(_ val: CInt)
// CHECK-NEXT:   var x: CInt
// CHECK-NEXT: }
// CHECK-NEXT: struct CopyAndMoveConstructor {
// CHECK-NEXT:   @available(*, deprecated, message
// CHECK-NEXT:   init()
// CHECK-NEXT:   init(value: CInt, ptr: UnsafeMutablePointer<CInt>!)
// CHECK-NEXT:   var value: CInt
// CHECK-NEXT:   var ptr: UnsafeMutablePointer<CInt>!
// CHECK-NEXT: }
// CHECK-NEXT: struct Base {
// CHECK-NEXT:   init()
// CHECK-NEXT: }
// CHECK-NEXT: struct ArgType {
// CHECK-NEXT:   init(i: CInt)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var i: CInt
// CHECK-NEXT: }
// CHECK-NEXT: struct HasVirtualBase {
// CHECK-NEXT:   init(_ Arg: ArgType)
// CHECK-NEXT:   var i: CInt
// CHECK-NEXT: }
// CHECK:      struct TemplatedConstructor {
// CHECK-NEXT:   @available(*, deprecated, message
// CHECK-NEXT:   init()
// CHECK-NEXT:   var value: ArgType
// CHECK-NEXT:   init<T>(_ value: T)
// CHECK-NEXT: }
// CHECK:      struct TemplatedConstructorWithExtraArg {
// CHECK-NEXT:   @available(*, deprecated, message
// CHECK-NEXT:   init()
// CHECK-NEXT:   init<T>(_: CInt, _ value: T)
// CHECK-NEXT:   init<T>(_ value: T, _: CInt)
// CHECK-NEXT:   init<T, U>(_ value: T, _ other: U)
// CHECK-NEXT: }
