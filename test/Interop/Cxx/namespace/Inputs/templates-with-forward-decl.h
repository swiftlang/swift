#ifndef TEST_INTEROP_CXX_NAMESPACE_INPUTS_TEMPLATES_WITH_FORWARD_DECL_H
#define TEST_INTEROP_CXX_NAMESPACE_INPUTS_TEMPLATES_WITH_FORWARD_DECL_H

namespace NS1 {

template <typename T>
struct Decl;

typedef Decl<int> di;

} // namespace NS1

namespace NS1 {

template <typename T>
struct ForwardDeclared;

template <typename T>
struct Decl {
  typedef T MyInt;
  ForwardDeclared<T> fwd;
  const static MyInt intValue = -1;
};

template <typename T>
const typename Decl<T>::MyInt Decl<T>::intValue;

} // namespace NS1

namespace NS1 {

template <typename T>
struct ForwardDeclared {};

} // namespace NS1

#endif // TEST_INTEROP_CXX_NAMESPACE_INPUTS_TEMPLATES_WITH_FORWARD_DECL_H
