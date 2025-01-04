#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_IN_NAMESPACE_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_IN_NAMESPACE_H

namespace Space {

template <class...> struct Ship;
template <class T, class... Args> struct Ship<T(Args...)> {};

using Orbiter = Ship<void(bool)>;

template <class T>
struct Box {
  T value;
};

using IntBoxWithinNS = Box<int>;
using BoxOfIntBoxWithinNS = Box<Box<int>>;

namespace NestedNS1 {
struct Impl {};
using ImplBox1 = Box<Impl>;
}

namespace NestedNS2 {
struct Impl {};
using ImplBox2 = Box<Impl>;
}

} // namespace Space

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_IN_NAMESPACE_H
