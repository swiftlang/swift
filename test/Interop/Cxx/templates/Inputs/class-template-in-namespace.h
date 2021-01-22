#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_IN_NAMESPACE_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_IN_NAMESPACE_H

struct Yolo {};

namespace Space {

template <class T> struct Ship {T t;};

} // namespace Space

namespace Engine {
    struct Turbojet {};
}

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_IN_NAMESPACE_H
