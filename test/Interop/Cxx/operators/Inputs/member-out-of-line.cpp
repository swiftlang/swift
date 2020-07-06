#include "member-out-of-line.h"

IntBox IntBox::operator+(IntBox rhs) {
  return IntBox{.value = value + rhs.value};
}
