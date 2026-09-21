// RUN: %target-swift-ide-test -print-module -module-to-print=MutabilityAnnotations -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: struct HasConstMethodAnnotatedAsMutating {
// CHECK:   var a: CInt
// CHECK:   mutating func annotatedMutating() -> CInt
// CHECK:   mutating func annotatedMutatingWithOtherAttrs() -> CInt
// CHECK: }

// CHECK: struct HasMutableProperty {
// CHECK:   var a: CInt
// CHECK:   var b: CInt
// CHECK:   func annotatedNonMutating() -> CInt
// TODO-CHECK:   mutating func noAnnotation() -> CInt
// CHECK:   mutating func contradictingAnnotations() -> CInt
// CHECK:   func duplicateAnnotations() -> CInt
// CHECK: }

// CHECK: struct NoMutableProperty {
// CHECK:   var a: CInt
// CHECK:   func isConst() -> CInt
// CHECK:   mutating func nonConst() -> CInt
// CHECK: }
