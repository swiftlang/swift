#include "member-out-of-line.h"

LoadableIntWrapper LoadableIntWrapper::operator+(LoadableIntWrapper rhs) const {
  return LoadableIntWrapper{.value = value + rhs.value};
}
