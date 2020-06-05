#include "non-member-out-of-line.h"

IntBox operator+(IntBox lhs, IntBox rhs) {
  return IntBox{.value = lhs.value + rhs.value};
}
