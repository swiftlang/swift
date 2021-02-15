#ifndef TEST_INTEROP_CXX_NAMESPACE_INPUTS_CLASSES_H
#define TEST_INTEROP_CXX_NAMESPACE_INPUTS_CLASSES_H

namespace ClassesNS1 {
struct BasicStruct {
  const char *basicMember() { return "ClassesNS1::BasicStruct::basicMember"; }
};
struct ForwardDeclaredStruct;
} // namespace ClassesNS1

struct ClassesNS1::ForwardDeclaredStruct {
  const char *basicMember() {
    return "ClassesNS1::ForwardDeclaredStruct::basicMember";
  }
};

namespace ClassesNS1 {
namespace ClassesNS2 {
struct BasicStruct {
  const char *basicMember() {
    return "ClassesNS1::ClassesNS2::BasicStruct::basicMember";
  }
};
struct ForwardDeclaredStruct;
struct DefinedInDefs;
} // namespace ClassesNS2
} // namespace ClassesNS1

namespace ClassesNS1 {
struct ClassesNS2::ForwardDeclaredStruct {
  const char *basicMember() {
    return "ClassesNS1::ClassesNS2::ForwardDeclaredStruct::basicMember";
  }
};
} // namespace ClassesNS1

namespace ClassesNS3 {
struct BasicStruct {
  const char *basicMember() { return "ClassesNS3::BasicStruct::basicMember"; }
};
} // namespace ClassesNS3

#endif // TEST_INTEROP_CXX_NAMESPACE_INPUTS_CLASSES_H
