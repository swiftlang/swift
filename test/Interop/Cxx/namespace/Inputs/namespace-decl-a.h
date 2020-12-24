#ifndef TEST_INTEROP_CXX_NAMESPACE_NAMESPACE_DECL_A_H
#define TEST_INTEROP_CXX_NAMESPACE_NAMESPACE_DECL_A_H

namespace outer {
namespace nested {
inline int getOne() { return 1; }
} //  namespace nested
} // namespace outer


namespace outer {
namespace nested {
inline int getTwo() { return 2; }
} //  namespace nested
} // namespace outer

#endif //  TEST_INTEROP_CXX_NAMESPACE_NAMESPACE_DECL_A_H
