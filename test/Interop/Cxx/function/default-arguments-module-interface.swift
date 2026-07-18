// RUN: %target-swift-ide-test -print-module -module-to-print=DefaultArguments -I %S/Inputs -source-filename=x -cxx-interoperability-mode=swift-5.9 | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=DefaultArguments -I %S/Inputs -source-filename=x -cxx-interoperability-mode=swift-6 | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=DefaultArguments -I %S/Inputs -source-filename=x -cxx-interoperability-mode=upcoming-swift | %FileCheck %s

// CHECK: func isZero(_ value: CInt = cxxDefaultArg) -> CBool
// CHECK: func isNil(_ ptr: UnsafeMutablePointer<CInt>! = cxxDefaultArg) -> CBool
// CHECK: func isGlobalNonNil(_ ptr: UnsafeMutablePointer<CInt>! = cxxDefaultArg) -> CBool
// CHECK: func sum(_ a: CInt, _ b: CInt = cxxDefaultArg) -> CInt
// CHECK: func subtract(_ a: CInt = cxxDefaultArg, _ b: CInt = cxxDefaultArg) -> CInt
// CHECK: func isArgZero(_ a: ArgTy = cxxDefaultArg) -> CBool
// CHECK: func isArgZeroOutOfLine(_ a: ArgTy = cxxDefaultArg) -> CBool

// CHECK: func isArgZeroConstRef(_ a: ArgTy) -> CBool
// CHECK: func isArgNonZeroConstRef(_ a: ArgTy) -> CBool
// CHECK: func isArgNonPODNonZeroConstRef(_ a: ArgTyNonPOD) -> CBool

// CHECK: func isArgViewNull(_ a: ArgTyView) -> CBool
// CHECK: func isArgViewNullAnd(_ a: ArgTyView, _ second: CBool = cxxDefaultArg) -> CBool
// CHECK: func isArgViewNullAndReversed(_ second: CBool = cxxDefaultArg, _ a: ArgTyView) -> CBool
// CHECK: func isArgViewNullUnsafeParam(_ a: ArgTyView = cxxDefaultArg) -> CBool
// CHECK: func isArgViewNullUnsafeFunc(_ a: ArgTyView) -> CBool
// CHECK: func isArgOwnedPtrNull(_ a: ArgTyOwnedPtr = cxxDefaultArg) -> CBool
// CHECK: func isArgFRTNull(_ a: ArgFRT! = cxxDefaultArg) -> CBool
// CHECK: func getArgFRTValue(_ a: ArgFRT! = cxxDefaultArg) -> CInt
// CHECK: func getArgRefCountedValue(_ a: ArgRefCounted! = cxxDefaultArg) -> CInt

// CHECK: struct HasMethodWithDefaultArg {
// CHECK:   func isZero(_ v: CInt = cxxDefaultArg) -> CBool
// CHECK:   func isNonZero(_ v: CInt = cxxDefaultArg) -> CBool
// CHECK:   func isNilPtr(_ v: UnsafeMutablePointer<CInt>! = cxxDefaultArg) -> CBool
// CHECK:   func isNilConstPtr(_ v: UnsafePointer<CInt>! = cxxDefaultArg) -> CBool
// CHECK:   func isZeroConstRef(_ v: CInt) -> CBool
// CHECK: }

// CHECK: struct DerivedFromHasMethodWithDefaultArg {
// CHECK:   func isZero(_ v: CInt = cxxDefaultArg) -> CBool
// CHECK: }

// CHECK: struct DerivedFromDerivedFromHasMethodWithDefaultArg {
// CHECK:   func isZero(_ v: CInt = cxxDefaultArg) -> CBool
// CHECK: }

// CHECK: struct HasStaticMethodWithDefaultArg {
// CHECK:   static func isNonZero(_ v: CInt = cxxDefaultArg) -> CBool
// CHECK:   static func isNonZeroCounter(_ v: CInt = cxxDefaultArg) -> CBool
// CHECK:   static func isNonZeroPrivateCounter(_ v: CInt = cxxDefaultArg) -> CBool
// CHECK:   static func isArgZeroRef(_ a: inout ArgTy) -> CBool
// CHECK: }

// CHECK: struct HasCtorWithDefaultArg {
// CHECK:   init(_ a: CInt, _ b: CInt = cxxDefaultArg, _ c: CInt = cxxDefaultArg)
// CHECK: }

// CHECK: struct TemplatedHasMethodWithDefaultArg<CFloat> {
// CHECK:   func isZero(_ v: CFloat = cxxDefaultArg) -> CBool
// CHECK:   func isNonZero(_ v: CFloat = cxxDefaultArg) -> CBool
// CHECK: }
// CHECK: struct TemplatedHasMethodWithDefaultArg<CInt> {
// CHECK:   func isZero(_ v: CInt = cxxDefaultArg) -> CBool
// CHECK:   func isNonZero(_ v: CInt = cxxDefaultArg) -> CBool
// CHECK: }

// CHECK: func ambiguous(_ a: CInt, _ b: CInt = cxxDefaultArg) -> CInt
// CHECK: func ambiguous(_ a: CInt) -> CInt

// CHECK: func nonTrailing(_ a: CInt = cxxDefaultArg, _ b: CInt = cxxDefaultArg) -> CInt

// CHECK: func takesUnnamedParam(_: CInt = cxxDefaultArg) -> CInt

// CHECK: struct InvalidStruct<NoDefinition> {
// CHECK:   func invalidDefaultExprMethod(_ x: Base<NoDefinition>)
// CHECK: }
// CHECK: typealias InvalidStructNoDef = InvalidStruct<NoDefinition>
