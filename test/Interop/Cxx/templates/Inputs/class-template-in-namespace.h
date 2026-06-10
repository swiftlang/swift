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

namespace NS1 {
template <typename T>
struct Box {};
using BoxInt = Box<int>;
} // namespace NS1

namespace NS2 {
template <typename T>
struct Box {};
using BoxInt = Box<int>;
} // namespace NS2

namespace A {
namespace B {
namespace C1 {

template <typename T>
struct Box {};
using BoxInt = Box<int>;
using BoxOfBoxInt = Box<Box<int>>;
using BoxOfBoxC1 = Box<Box<int>>;

} // namespace C1

namespace C2 {

template <typename T>
struct Box {};
using BoxInt = Box<int>;

} // namespace C2

template <typename T>
struct Box {};
using BoxInt = Box<int>;
using BoxOfBoxInt = Box<Box<int>>;
using BoxOfBoxC1 = Box<C1::Box<int>>;
using BoxOfBoxC2 = Box<C2::Box<int>>;

} // namespace B
} // namespace A

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_IN_NAMESPACE_H
