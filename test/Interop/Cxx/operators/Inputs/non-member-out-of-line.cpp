#include "non-member-out-of-line.h"

LoadableIntWrapper operator+(LoadableIntWrapper lhs, LoadableIntWrapper rhs) {
  return LoadableIntWrapper{.value = lhs.value + rhs.value};
}

bool operator==(const ClassWithOperatorEqualsParamUnnamed &lhs,
                const ClassWithOperatorEqualsParamUnnamed &rhs) {
  return false;
}

bool operator!=(const ClassWithOperatorEqualsParamUnnamed &,
                const ClassWithOperatorEqualsParamUnnamed &) {
  return true;
}
