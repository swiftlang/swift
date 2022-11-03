// RUN: %target-swift-ide-test -print-module -module-to-print=Templates -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK:     enum TemplatesNS1 {
// CHECK-NEXT:   enum TemplatesNS2 {
// CHECK-NEXT:     static func forwardDeclaredFunctionTemplate<T>(_: T) -> UnsafePointer<CChar>!
// CHECK-NEXT:     struct __CxxTemplateInstN12TemplatesNS112TemplatesNS228ForwardDeclaredClassTemplateIcEE {
// CHECK-NEXT:       init()
// CHECK-NEXT:       mutating func basicMember() -> UnsafePointer<CChar>!
// CHECK-NEXT:     }
// CHECK-NEXT:     @available(*, unavailable, message: "Un-specialized class templates are not currently supported. Please use a specialization of this type.")
// CHECK-NEXT:     struct ForwardDeclaredClassTemplate<> {
// CHECK-NEXT:     }
// CHECK-NEXT:     static func forwardDeclaredFunctionTemplateOutOfLine<T>(_: T) -> UnsafePointer<CChar>!
// CHECK-NEXT:     struct __CxxTemplateInstN12TemplatesNS112TemplatesNS237ForwardDeclaredClassTemplateOutOfLineIcEE {
// CHECK-NEXT:       init()
// CHECK-NEXT:       mutating func basicMember() -> UnsafePointer<CChar>!
// CHECK-NEXT:     }
// CHECK-NEXT:     @available(*, unavailable, message: "Un-specialized class templates are not currently supported. Please use a specialization of this type.")
// CHECK-NEXT:     struct ForwardDeclaredClassTemplateOutOfLine<> {
// CHECK-NEXT:     }
// CHECK-NEXT:     typealias BasicClassTemplateChar = TemplatesNS1.TemplatesNS3.__CxxTemplateInstN12TemplatesNS112TemplatesNS318BasicClassTemplateIcEE
// CHECK-NEXT:     static func takesClassTemplateFromSibling(_: TemplatesNS1.TemplatesNS2.BasicClassTemplateChar) -> UnsafePointer<CChar>!
// CHECK-NEXT:   }
// CHECK-NEXT:   static func basicFunctionTemplate<T>(_: T) -> UnsafePointer<CChar>!
// CHECK-NEXT:   struct __CxxTemplateInstN12TemplatesNS118BasicClassTemplateIcEE {
// CHECK-NEXT:     init()
// CHECK-NEXT:     mutating func basicMember() -> UnsafePointer<CChar>!
// CHECK-NEXT:   }
// CHECK-NEXT:   @available(*, unavailable, message: "Un-specialized class templates are not currently supported. Please use a specialization of this type.")
// CHECK-NEXT:   struct BasicClassTemplate<> {
// CHECK-NEXT:   }
// CHECK-NEXT:   typealias BasicClassTemplateChar = TemplatesNS1.__CxxTemplateInstN12TemplatesNS118BasicClassTemplateIcEE
// CHECK-NEXT:   static func basicFunctionTemplateDefinedInDefs<T>(_: T) -> UnsafePointer<CChar>!
// CHECK-NEXT:   @available(*, unavailable, message: "Un-specialized class templates are not currently supported. Please use a specialization of this type.")
// CHECK-NEXT:   struct BasicClassTemplateDefinedInDefs<> {
// CHECK-NEXT:   }
// CHECK-NEXT:   typealias UseTemplate = TemplatesNS4.__CxxTemplateInstN12TemplatesNS417HasSpecializationIcEE
// CHECK-NEXT:   typealias UseSpecialized = TemplatesNS4.__CxxTemplateInstN12TemplatesNS417HasSpecializationIiEE
// CHECK-NEXT:   enum TemplatesNS3 {
// CHECK-NEXT:     struct __CxxTemplateInstN12TemplatesNS112TemplatesNS318BasicClassTemplateIcEE {
// CHECK-NEXT:       init()
// CHECK-NEXT:     }
// CHECK-NEXT:     @available(*, unavailable, message: "Un-specialized class templates are not currently supported. Please use a specialization of this type.")
// CHECK-NEXT:     struct BasicClassTemplate<> {
// CHECK-NEXT:     }
// CHECK-NEXT:   }
// CHECK-NEXT:   struct __CxxTemplateInstN12TemplatesNS112TemplatesNS228ForwardDeclaredClassTemplateIcEE {
// CHECK-NEXT:     init()
// CHECK-NEXT:     mutating func basicMember() -> UnsafePointer<CChar>!
// CHECK-NEXT:   }
// CHECK-NEXT:   typealias ForwardDeclaredClassTemplateChar = TemplatesNS1.TemplatesNS2.__CxxTemplateInstN12TemplatesNS112TemplatesNS228ForwardDeclaredClassTemplateIcEE
// CHECK-NEXT: }
// CHECK-NEXT: typealias ForwardDeclaredClassTemplateOutOfLineChar = TemplatesNS1.TemplatesNS2.__CxxTemplateInstN12TemplatesNS112TemplatesNS237ForwardDeclaredClassTemplateOutOfLineIcEE
// CHECK-NEXT: enum TemplatesNS4 {
// CHECK-NEXT:   struct __CxxTemplateInstN12TemplatesNS417HasSpecializationIcEE {
// CHECK-NEXT:     init()
// CHECK-NEXT:   }
// CHECK-NEXT:   struct __CxxTemplateInstN12TemplatesNS417HasSpecializationIiEE {
// CHECK-NEXT:     init()
// CHECK-NEXT:   }
// CHECK-NEXT:   @available(*, unavailable, message: "Un-specialized class templates are not currently supported. Please use a specialization of this type.")
// CHECK-NEXT:   struct HasSpecialization<> {
// CHECK-NEXT:   }
// CHECK-NEXT: }
