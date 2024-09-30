#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_MEMBERS_DIRECT_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_MEMBERS_DIRECT_H

#include <members-transitive.h>

namespace DirectNamespace { }

namespace CommonNamespace {

struct DirectStruct {
  int memberVar;
};

} // namespace CommonNamespace

CommonNamespace::TransitiveStruct returnsTransitiveStruct() {
  return CommonNamespace::TransitiveStruct{};
}

TopLevelTransitiveStruct returnsTopLevelTransitiveStruct() {
  return TopLevelTransitiveStruct{};
}

#endif // TEST_INTEROP_CXX_CLASS_INPUTS_MEMBERS_DIRECT_H
