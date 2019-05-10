
#ifndef chained_unit_test_bridging_header_to_pch_h
#define chained_unit_test_bridging_header_to_pch_h

#include "app-bridging-header-to-pch.h"

static inline int unit_test_function(int x) {
  return x + 28;
}

#endif

