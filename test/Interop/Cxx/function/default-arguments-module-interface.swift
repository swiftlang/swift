// RUN: %target-swift-ide-test -print-module -module-to-print=DefaultArguments -I %S/Inputs -source-filename=x -cxx-interoperability-mode=swift-5.9 | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=DefaultArguments -I %S/Inputs -source-filename=x -cxx-interoperability-mode=swift-6 | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=DefaultArguments -I %S/Inputs -source-filename=x -cxx-interoperability-mode=upcoming-swift | %FileCheck %s

// CHECK: func isZero(_ value: Int32 = cxxDefaultArg) -> Bool
// CHECK: func isNil(_ ptr: UnsafeMutablePointer<Int32>! = cxxDefaultArg) -> Bool
// CHECK: func isGlobalNonNil(_ ptr: UnsafeMutablePointer<Int32>! = cxxDefaultArg) -> Bool
// CHECK: func sum(_ a: Int32, _ b: Int32 = cxxDefaultArg) -> Int32
// CHECK: func subtract(_ a: Int32 = cxxDefaultArg, _ b: Int32 = cxxDefaultArg) -> Int32
// CHECK: func isArgZero(_ a: ArgTy = cxxDefaultArg) -> Bool
// CHECK: func isArgZeroOutOfLine(_ a: ArgTy = cxxDefaultArg) -> Bool

// CHECK: func isArgZeroConstRef(_ a: ArgTy) -> Bool
// CHECK: func isArgNonZeroConstRef(_ a: ArgTy) -> Bool
// CHECK: func isArgNonPODNonZeroConstRef(_ a: ArgTyNonPOD) -> Bool

// CHECK: func isArgViewNull(_ a: ArgTyView) -> Bool
// CHECK: func isArgViewNullAnd(_ a: ArgTyView, _ second: Bool = cxxDefaultArg) -> Bool
// CHECK: func isArgViewNullAndReversed(_ second: Bool = cxxDefaultArg, _ a: ArgTyView) -> Bool
// CHECK: func isArgViewNullUnsafeParam(_ a: ArgTyView = cxxDefaultArg) -> Bool
// CHECK: func isArgViewNullUnsafeFunc(_ a: ArgTyView) -> Bool
// CHECK: func isArgOwnedPtrNull(_ a: ArgTyOwnedPtr = cxxDefaultArg) -> Bool
// CHECK: func isArgFRTNull(_ a: ArgFRT! = cxxDefaultArg) -> Bool
// CHECK: func getArgFRTValue(_ a: ArgFRT! = cxxDefaultArg) -> Int32
// CHECK: func getArgRefCountedValue(_ a: ArgRefCounted! = cxxDefaultArg) -> Int32

// CHECK: struct HasMethodWithDefaultArg {
// CHECK:   func isZero(_ v: Int32 = cxxDefaultArg) -> Bool
// CHECK:   func isNonZero(_ v: Int32 = cxxDefaultArg) -> Bool
// CHECK:   func isNilPtr(_ v: UnsafeMutablePointer<Int32>! = cxxDefaultArg) -> Bool
// CHECK:   func isNilConstPtr(_ v: UnsafePointer<Int32>! = cxxDefaultArg) -> Bool
// CHECK:   func isZeroConstRef(_ v: Int32) -> Bool
// CHECK: }

// CHECK: struct DerivedFromHasMethodWithDefaultArg {
// CHECK:   func isZero(_ v: Int32 = cxxDefaultArg) -> Bool
// CHECK: }

// CHECK: struct DerivedFromDerivedFromHasMethodWithDefaultArg {
// CHECK:   func isZero(_ v: Int32 = cxxDefaultArg) -> Bool
// CHECK: }

// CHECK: struct HasStaticMethodWithDefaultArg {
// CHECK:   static func isNonZero(_ v: Int32 = cxxDefaultArg) -> Bool
// CHECK:   static func isNonZeroCounter(_ v: Int32 = cxxDefaultArg) -> Bool
// CHECK:   static func isNonZeroPrivateCounter(_ v: Int32 = cxxDefaultArg) -> Bool
// CHECK:   static func isArgZeroRef(_ a: inout ArgTy) -> Bool
// CHECK: }

// CHECK: struct HasCtorWithDefaultArg {
// TODO: support default arguments of constructors (https://github.com/apple/swift/issues/70124)
// TODO:   init(_ a: Int32, _ b: Int32 = cxxDefaultArg, _ c: Int32 = cxxDefaultArg)
// CHECK: }

// CHECK: struct TemplatedHasMethodWithDefaultArg<CFloat> {
// CHECK:   func isZero(_ v: Float = cxxDefaultArg) -> Bool
// CHECK:   func isNonZero(_ v: Float = cxxDefaultArg) -> Bool
// CHECK: }
// CHECK: struct TemplatedHasMethodWithDefaultArg<CInt> {
// CHECK:   func isZero(_ v: Int32 = cxxDefaultArg) -> Bool
// CHECK:   func isNonZero(_ v: Int32 = cxxDefaultArg) -> Bool
// CHECK: }

// CHECK: func ambiguous(_ a: Int32, _ b: Int32 = cxxDefaultArg) -> Int32
// CHECK: func ambiguous(_ a: Int32) -> Int32

// CHECK: func nonTrailing(_ a: Int32 = cxxDefaultArg, _ b: Int32 = cxxDefaultArg) -> Int32

// CHECK: struct InvalidStruct<NoDefinition> {
// CHECK:   func invalidDefaultExprMethod(_ x: Base<NoDefinition>)
// CHECK: }
// CHECK: typealias InvalidStructNoDef = InvalidStruct<NoDefinition>
