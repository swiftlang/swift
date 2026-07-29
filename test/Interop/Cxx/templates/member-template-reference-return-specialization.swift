// Regression test for a C++ member function template whose type parameter
// appears only in a *reference* return type.
//
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
//
// (1) Call sites must typecheck when T is inferred from contextual pointee type
//
// RUN: %target-swift-frontend -typecheck -verify -suppress-notes \
// RUN:     -cxx-interoperability-mode=default \
// RUN:     -I %t%{fs-sep}Inputs %t%{fs-sep}main.swift
//
// (2) The generic member template is imported its type parameter
//
// RUN: %target-swift-ide-test -print-module -cxx-interoperability-mode=default \
// RUN:     -module-to-print=Repro -I %t%{fs-sep}Inputs -source-filename=x \
// RUN:   | %FileCheck %s

//--- Inputs/module.modulemap
module Repro {
  header "repro.h"
  requires cplusplus
  export *
}

//--- Inputs/repro.h
#pragma once

struct Value {
  void *storage;

  Value() : storage(nullptr) {}
  ~Value() {}

  // User-declared copy ctor => "self-contained" => reference-returning members
  // are treated as unsafe and renamed to __<name>Unsafe
  Value(const Value &other) : storage(other.storage) {}

  // This will not be explicitly specialized
  template <class T>
  const T &GetNoSpec() const & { return *static_cast<const T *>(storage); }

  // This will be explicitly specialized
  template <class T>
  const T &GetWithSpec() const & { return *static_cast<const T *>(storage); }
};

// CHECK: struct Value {
// CHECK:   func __GetNoSpecUnsafe<T>() -> UnsafePointer<T>
// CHECK:   func __GetWithSpecUnsafe<T>() -> UnsafePointer<T>
// CHECK: }

// Explicit full specialization for T = Value (mirrors value.h:1542)
template <>
inline const Value &Value::GetWithSpec<Value>() const & { return *this; }

//--- main.swift
import Repro

func f(v: Value) {
  let _: Bool = v.__GetNoSpecUnsafe().pointee
  let _: Bool = v.__GetWithSpecUnsafe().pointee

  let _: Value = v.__GetNoSpecUnsafe().pointee
  let _: Value = v.__GetWithSpecUnsafe().pointee
}
