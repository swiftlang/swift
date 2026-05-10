#ifndef TEST_INTEROP_CXX_NAMESPACE_INPUTS_FREE_FUNCTION_H
#define TEST_INTEROP_CXX_NAMESPACE_INPUTS_FREE_FUNCTION_H

namespace FunctionsNS1 {
inline const char *basicFunctionTopLevel() {
  return "FunctionsNS1::basicFunctionTopLevel";
}
inline const char *forwardDeclared();
inline const char *definedOutOfLine();

struct X {};
inline const char *operator+(X, X) { return "FunctionsNS1::operator+(X, X)"; }
} // namespace FunctionsNS1

namespace FunctionsNS1 {
inline const char *forwardDeclared() { return "FunctionsNS1::forwardDeclared"; }
} // namespace FunctionsNS1

inline const char *FunctionsNS1::definedOutOfLine() {
  return "FunctionsNS1::definedOutOfLine";
}

namespace FunctionsNS1 {
namespace FunctionsNS2 {
inline const char *basicFunctionSecondLevel() {
  return "FunctionsNS1::FunctionsNS2::basicFunctionSecondLevel";
}
} // namespace FunctionsNS2
} // namespace FunctionsNS1

namespace FunctionsNS1 {
namespace FunctionsNS2 {
namespace FunctionsNS3 {
inline const char *basicFunctionLowestLevel() {
  return "FunctionsNS1::FunctionsNS2::FunctionsNS3::basicFunctionLowestLevel";
}
} // namespace FunctionsNS3
} // namespace FunctionsNS2
} // namespace FunctionsNS1

namespace FunctionsNS1 {
inline const char *definedInDefs();
}

namespace FunctionsNS1 {
inline const char *sameNameInChild() { return "FunctionsNS1::sameNameInChild"; }
inline const char *sameNameInSibling() {
  return "FunctionsNS1::sameNameInSibling";
}
namespace FunctionsNS2 {
inline const char *sameNameInChild() {
  return "FunctionsNS1::FunctionsNS2::sameNameInChild";
}
} // namespace FunctionsNS2
} // namespace FunctionsNS1

namespace FunctionsNS4 {
inline const char *sameNameInSibling() {
  return "FunctionsNS4::sameNameInSibling";
}
} // namespace FunctionsNS4

namespace FunctionsNS1 {
namespace FunctionsNS2 {
namespace FunctionsNS3 {
struct Y {};
inline bool operator==(Y, Y) { return true; }
} // namespace FunctionsNS3
} // namespace FunctionsNS2
} // namespace FunctionsNS1

#endif // TEST_INTEROP_CXX_NAMESPACE_INPUTS_FREE_FUNCTION_H
