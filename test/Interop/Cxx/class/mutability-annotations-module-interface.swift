// RUN: %target-swift-ide-test -print-module -module-to-print=MutabilityAnnotations -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: struct HasConstMethodAnnotatedAsMutating {
// CHECK:   mutating func annotatedMutating() -> Int32
// CHECK:   mutating func annotatedMutatingWithOtherAttrs() -> Int32
// CHECK:   var a: Int32
// CHECK: }
