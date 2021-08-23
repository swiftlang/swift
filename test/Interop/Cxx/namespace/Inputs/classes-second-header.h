#ifndef TEST_INTEROP_CXX_NAMESPACE_INPUTS_CLASSES_SECOND_HEADER_H
#define TEST_INTEROP_CXX_NAMESPACE_INPUTS_CLASSES_SECOND_HEADER_H

#include "classes.h"

struct ClassesNS1::ClassesNS2::DefinedInDefs {
  const char *basicMember() {
    return "ClassesNS1::ClassesNS2::DefinedInDefs::basicMember";
  }
};

#endif // TEST_INTEROP_CXX_NAMESPACE_INPUTS_CLASSES_SECOND_HEADER_H
