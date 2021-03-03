#include "member-out-of-line.h"

LoadableIntWrapper LoadableIntWrapper::operator+(LoadableIntWrapper rhs) const {
  return LoadableIntWrapper{.value = value + rhs.value};
}

int LoadableIntWrapper::operator()() const {
  return value;
}

int LoadableIntWrapper::operator()(int x) const {
  return value + x;
}

int LoadableIntWrapper::operator()(int x, int y) const {
  return value + x * y;
}

int AddressOnlyIntWrapper::operator()() const {
  return value;
}

int AddressOnlyIntWrapper::operator()(int x) const {
  return value + x;
}

int AddressOnlyIntWrapper::operator()(int x, int y) const {
  return value + x * y;
}
