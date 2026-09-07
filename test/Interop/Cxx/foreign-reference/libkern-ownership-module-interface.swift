// RUN: %target-swift-ide-test -print-module -cxx-interoperability-mode=default -print-implicit-attrs -module-to-print=LibkernOwnership -I %S/Inputs -source-filename=x | %FileCheck %s

// CHECK: class Service {
// CHECK:   @available(*, unavailable, message: "LIBKERN_CONSUMED annotation is not supported")
// CHECK:   class func consumesService(_ service: Service!)
// CHECK:   @available(*, unavailable, message: "LIBKERN_CONSUMES_THIS annotation is not supported")
// CHECK:   func consumeMyself()
// CHECK: }

// CHECK: class DerivedService {
// CHECK:   @available(*, unavailable, message: "LIBKERN_CONSUMES_THIS annotation is not supported")
// CHECK:   func consumeMyself()
// CHECK: }
