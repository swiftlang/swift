#include "member-out-of-line.h"

IntBox IntBox::operator+(IntBox rhs) const {
  return IntBox{.value = value + rhs.value};
}
