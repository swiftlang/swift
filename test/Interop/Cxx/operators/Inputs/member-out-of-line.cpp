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

const int& ReadWriteIntArray::operator[](int x) const {
  return values[x];
}

int& ReadWriteIntArray::operator[](int x) {
  return values[x];
}

int NonTrivialIntArrayByVal::operator[](int x) {
  return values[x];
}

bool ClassWithOperatorEqualsParamUnnamed::operator==(
    const ClassWithOperatorEqualsParamUnnamed &other) const {
  return false;
}

bool ClassWithOperatorEqualsParamUnnamed::operator!=(
    const ClassWithOperatorEqualsParamUnnamed &) const {
  return true;
}
