#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_IN_NAMESPACE_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_IN_NAMESPACE_H

namespace Space {

template <class...> struct Ship;
template <class T, class... Args> struct Ship<T(Args...)> {};

using Orbiter = Ship<void(bool)>;

} // namespace Space

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_IN_NAMESPACE_H

// namespace Space {
//     template <class T> struct Ship { T t; };
// } // namespace Space

// namespace Engine {
//     struct Turbojet {};
// } // namespace Engine

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