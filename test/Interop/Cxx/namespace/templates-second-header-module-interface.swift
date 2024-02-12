// RUN: %target-swift-ide-test -print-module -module-to-print=TemplatesSecondHeader -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: enum TemplatesNS1 {
// CHECK:   static func basicFunctionTemplateDefinedInDefs<T>(_: T) -> UnsafePointer<CChar>!
// CHECK:   struct BasicClassTemplateDefinedInDefs<CChar> {
// CHECK:     init()
// CHECK:     mutating func basicMember() -> UnsafePointer<CChar>!
// CHECK:   }
// CHECK: }

// CHECK: typealias BasicClassTemplateDefinedInDefsChar = TemplatesNS1.BasicClassTemplateDefinedInDefs<CChar>
