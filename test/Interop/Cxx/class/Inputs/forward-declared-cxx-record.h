#ifndef TEST_INTEROP_CXX_CLASS_INPUTS_FORWARD_DECLARED_CXX_RECORD_H
#define TEST_INTEROP_CXX_CLASS_INPUTS_FORWARD_DECLARED_CXX_RECORD_H

#include "forward-declared-cxx-record-used.h"

struct __attribute__((swift_attr("import_as_ref"))) ForwardDeclaredInOtherHeader {
  ~ForwardDeclaredInOtherHeader() = delete;
};

#endif // TEST_INTEROP_CXX_CLASS_INPUTS_FORWARD_DECLARED_CXX_RECORD_H
