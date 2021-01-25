#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_IN_NAMESPACE_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_IN_NAMESPACE_H

namespace Space {
    template <class T> struct Ship { T t; };
} // namespace Space

namespace Engine {
    struct Turbojet {};
} // namespace Engine

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_IN_NAMESPACE_H
