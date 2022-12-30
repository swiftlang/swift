// RUN: %target-swift-ide-test -print-module -module-to-print=FreeFunctions -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK:      enum FunctionsNS1 {
// CHECK-NEXT:   static func basicFunctionTopLevel() -> UnsafePointer<CChar>!
// CHECK-NEXT:   static func forwardDeclared() -> UnsafePointer<CChar>!
// CHECK-NEXT:   static func definedOutOfLine() -> UnsafePointer<CChar>!
// CHECK-NEXT:   struct X {
// CHECK-NEXT:     init()
// CHECK-NEXT:   }

// FIXME: this seems wrong, the operator shouldn't be printed twice (https://github.com/apple/swift/issues/62727).
// CHECK-NEXT:   func + (_: FunctionsNS1.X, _: FunctionsNS1.X) -> UnsafePointer<CChar>!

// CHECK-NEXT:   enum FunctionsNS2 {
// CHECK-NEXT:     enum FunctionsNS3 {
// CHECK-NEXT:       struct Y {
// CHECK-NEXT:         init()
// CHECK-NEXT:       }

// FIXME: this seems wrong, the operator shouldn't be printed twice (https://github.com/apple/swift/issues/62727).
// CHECK-NEXT: func == (_: FunctionsNS1.FunctionsNS2.FunctionsNS3.Y, _: FunctionsNS1.FunctionsNS2.FunctionsNS3.Y) -> Bool

// CHECK-NEXT:       static func basicFunctionLowestLevel() -> UnsafePointer<CChar>!
// CHECK-NEXT:     }
// CHECK-NEXT:     static func sameNameInChild() -> UnsafePointer<CChar>!
// CHECK-NEXT:     static func basicFunctionSecondLevel() -> UnsafePointer<CChar>!
// CHECK-NEXT:   }

// CHECK-NEXT:   static func sameNameInChild() -> UnsafePointer<CChar>!
// CHECK-NEXT:   static func sameNameInSibling() -> UnsafePointer<CChar>!
// CHECK-NEXT:   static func definedInDefs() -> UnsafePointer<CChar>!
// CHECK-NEXT: }

// CHECK-NEXT: func + (_: FunctionsNS1.X, _: FunctionsNS1.X) -> UnsafePointer<CChar>!

// CHECK-NEXT: enum FunctionsNS4 {
// CHECK-NEXT:   static func sameNameInSibling() -> UnsafePointer<CChar>!
// CHECK-NEXT: }

// CHECK-NEXT: func == (_: FunctionsNS1.FunctionsNS2.FunctionsNS3.Y, _: FunctionsNS1.FunctionsNS2.FunctionsNS3.Y) -> Bool
