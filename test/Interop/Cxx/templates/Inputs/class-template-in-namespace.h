#ifndef TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_IN_NAMESPACE_H
#define TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_IN_NAMESPACE_H

namespace Space {

template <class...> struct Ship;
template <class T, class... Args> struct Ship<T(Args...)> {};

using Orbiter = Ship<void(bool)>;

} // namespace Space

#endif // TEST_INTEROP_CXX_TEMPLATES_INPUTS_CLASS_TEMPLATE_IN_NAMESPACE_H
