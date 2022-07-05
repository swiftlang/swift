// RUN: %target-swift-ide-test -print-module -module-to-print=MutabilityAnnotations -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: struct HasConstMethodAnnotatedAsMutating {
// CHECK:   mutating func annotatedMutating() -> Int32
// CHECK:   mutating func annotatedMutatingWithOtherAttrs() -> Int32
// CHECK:   var a: Int32
// CHECK: }

// CHECK: struct HasMutableProperty {
// CHECK:   func annotatedNonMutating() -> Int32
// TODO-CHECK:   mutating func noAnnotation() -> Int32
// CHECK:   mutating func contradictingAnnotations() -> Int32
// CHECK:   func duplicateAnnotations() -> Int32
// CHECK:   var a: Int32
// CHECK:   var b: Int32
// CHECK: }

// CHECK: struct NoMutableProperty {
// CHECK:   func isConst() -> Int32
// CHECK:   mutating func nonConst() -> Int32
// CHECK:   var a: Int32
// CHECK: }
