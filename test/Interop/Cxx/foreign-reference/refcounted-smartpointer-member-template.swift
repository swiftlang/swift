// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify %t/test.swift \
// RUN:   -I %t/Inputs -cxx-interoperability-mode=default \
// RUN:   -disable-availability-checking

//--- Inputs/module.modulemap
module Test {
  header "test.h"
  requires cplusplus
}

//--- Inputs/test.h
#pragma once

#include "swift/bridging"

struct SWIFT_SHARED_REFERENCE(retainObject, releaseObject) Object {
  int refCount = 1;
};

void retainObject(Object *_Nonnull);
void releaseObject(Object *_Nonnull);

template <typename T>
class SWIFT_REFCOUNTED_PTR(.get) Ptr {
public:
  Ptr(T &object) : m_ptr(&object) {}
  Ptr(const Ptr &other) : m_ptr(other.m_ptr) {}
  ~Ptr() {}

  T &get() const { return *m_ptr; }

  template <typename U>
  void take(Ptr<T>, U);

  template <typename U>
  Ptr<T> replace(Ptr<U> &&);

  Ptr copy() const { return *this; }

private:
  T *_Nonnull m_ptr;
};

Ptr<Object> makeObject();

//--- test.swift
import Test

func test() -> Object {
  return makeObject()
}
