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

namespace GlobalAliasToNS1 = ClassesNS1;

namespace ClassesNS4 {
namespace AliasToGlobalNS1 = ::ClassesNS1;
namespace AliasToGlobalNS2 = ::ClassesNS1::ClassesNS2;

namespace ClassesNS5 {
struct BasicStruct {};
} // namespace ClassesNS5

namespace AliasToInnerNS5 = ClassesNS5;
namespace AliasToNS2 = ClassesNS1::ClassesNS2;

namespace AliasChainToNS1 = GlobalAliasToNS1;
namespace AliasChainToNS2 = AliasChainToNS1::ClassesNS2;
} // namespace ClassesNS4

namespace ClassesNS5 {
struct BasicStruct {};
namespace AliasToAnotherNS5 = ClassesNS4::ClassesNS5;

namespace ClassesNS5 {
struct BasicStruct {};
namespace AliasToNS5NS5 = ClassesNS5;
} // namespace ClassesNS5

namespace AliasToGlobalNS5 = ::ClassesNS5;
namespace AliasToLocalNS5 = ClassesNS5;
namespace AliasToNS5 = ::ClassesNS5::ClassesNS5;
} // namespace ClassesNS5

#endif // TEST_INTEROP_CXX_NAMESPACE_INPUTS_CLASSES_H
