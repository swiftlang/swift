// RUN: %target-swift-ide-test -print-module -module-to-print=Templates -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK:     enum TemplatesNS1 {
// CHECK-NEXT:   enum TemplatesNS2 {
// CHECK-NEXT:     static func forwardDeclaredFunctionTemplate<T>(_: T) -> UnsafePointer<CChar>!
// CHECK-NEXT:     struct ForwardDeclaredClassTemplate<CChar> {
// CHECK-NEXT:       init()
// CHECK-NEXT:       mutating func basicMember() -> UnsafePointer<CChar>!
// CHECK-NEXT:     }
// CHECK-NEXT:     @available(*, unavailable, message: "Un-specialized class templates are not currently supported. Please use a specialization of this type.")
// CHECK-NEXT:     struct ForwardDeclaredClassTemplate<> {
// CHECK-NEXT:     }
// CHECK-NEXT:     static func forwardDeclaredFunctionTemplateOutOfLine<T>(_: T) -> UnsafePointer<CChar>!
// CHECK-NEXT:     struct ForwardDeclaredClassTemplateOutOfLine<CChar> {
// CHECK-NEXT:       init()
// CHECK-NEXT:       mutating func basicMember() -> UnsafePointer<CChar>!
// CHECK-NEXT:     }
// CHECK-NEXT:     @available(*, unavailable, message: "Un-specialized class templates are not currently supported. Please use a specialization of this type.")
// CHECK-NEXT:     struct ForwardDeclaredClassTemplateOutOfLine<> {
// CHECK-NEXT:     }
// CHECK-NEXT:     typealias BasicClassTemplateChar = TemplatesNS1.TemplatesNS3.BasicClassTemplate<CChar>
// CHECK-NEXT:     static func takesClassTemplateFromSibling(_: TemplatesNS1.TemplatesNS2.BasicClassTemplateChar) -> UnsafePointer<CChar>!
// CHECK-NEXT:   }
// CHECK-NEXT:   static func basicFunctionTemplate<T>(_: T) -> UnsafePointer<CChar>!
// CHECK-NEXT:   struct BasicClassTemplate<CChar> {
// CHECK-NEXT:     init()
// CHECK-NEXT:     mutating func basicMember() -> UnsafePointer<CChar>!
// CHECK-NEXT:   }
// CHECK-NEXT:   @available(*, unavailable, message: "Un-specialized class templates are not currently supported. Please use a specialization of this type.")
// CHECK-NEXT:   struct BasicClassTemplate<> {
// CHECK-NEXT:   }
// CHECK-NEXT:   typealias BasicClassTemplateChar = TemplatesNS1.BasicClassTemplate<CChar>
// CHECK-NEXT:   static func basicFunctionTemplateDefinedInDefs<T>(_: T) -> UnsafePointer<CChar>!
// CHECK-NEXT:   @available(*, unavailable, message: "Un-specialized class templates are not currently supported. Please use a specialization of this type.")
// CHECK-NEXT:   struct BasicClassTemplateDefinedInDefs<> {
// CHECK-NEXT:   }
// CHECK-NEXT:   typealias UseTemplate = TemplatesNS4.HasSpecialization<CChar>
// CHECK-NEXT:   typealias UseSpecialized = TemplatesNS4.HasSpecialization<CInt>
// CHECK-NEXT:   enum TemplatesNS3 {
// CHECK-NEXT:     struct BasicClassTemplate<CChar> {
// CHECK-NEXT:       init()
// CHECK-NEXT:     }
// CHECK-NEXT:     @available(*, unavailable, message: "Un-specialized class templates are not currently supported. Please use a specialization of this type.")
// CHECK-NEXT:     struct BasicClassTemplate<> {
// CHECK-NEXT:     }
// CHECK-NEXT:   }
// CHECK-NEXT:   struct ForwardDeclaredClassTemplate<CChar> {
// CHECK-NEXT:     init()
// CHECK-NEXT:     mutating func basicMember() -> UnsafePointer<CChar>!
// CHECK-NEXT:   }
// CHECK-NEXT:   typealias ForwardDeclaredClassTemplateChar = TemplatesNS1.TemplatesNS2.ForwardDeclaredClassTemplate<CChar>
// CHECK-NEXT: }
// CHECK-NEXT: typealias ForwardDeclaredClassTemplateOutOfLineChar = TemplatesNS1.TemplatesNS2.ForwardDeclaredClassTemplateOutOfLine<CChar>
// CHECK-NEXT: enum TemplatesNS4 {
// CHECK-NEXT:   struct HasSpecialization<CChar> {
// CHECK-NEXT:     init()
// CHECK-NEXT:   }
// CHECK-NEXT:   struct HasSpecialization<CInt> {
// CHECK-NEXT:     init()
// CHECK-NEXT:   }
// CHECK-NEXT:   @available(*, unavailable, message: "Un-specialized class templates are not currently supported. Please use a specialization of this type.")
// CHECK-NEXT:   struct HasSpecialization<> {
// CHECK-NEXT:   }
// CHECK-NEXT: }
