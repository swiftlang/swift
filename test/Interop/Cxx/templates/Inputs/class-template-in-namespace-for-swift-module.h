#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_IN_NAMESPACE_FOR_SWIFT_MODULE_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_IN_NAMESPACE_FOR_SWIFT_MODULE_H

namespace Space {
    template <class T> struct Ship { T t; };
} // namespace Space

namespace Engine {
    struct Turbojet {};
} // namespace Engine

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_IN_NAMESPACE_FOR_SWIFT_MODULE_H

// RUN: %target-swift-ide-test -print-module -module-to-print=ClassTemplateInNamespace -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: enum Space {
// CHECK:   struct Ship<T> {
// CHECK:   }
// CHECK: }
// CHECK: enum Engine {
// CHECK:   struct Turbojet {
// CHECK:     init()
// CHECK:   }
// CHECK: }