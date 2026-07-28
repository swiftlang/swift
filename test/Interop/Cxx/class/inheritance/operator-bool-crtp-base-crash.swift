// Regression test for a spurious Clang error when importing a C++ class that
// inherits `operator bool()`, where the derived class's definition is not
// reachable from Swift.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
//
// RUN: %target-swift-frontend -typecheck -verify -suppress-notes \
// RUN:   %t/test.swift -I %t/Inputs -module-name Repro \
// RUN:   -cxx-interoperability-mode=default \
// RUN:   -import-objc-header %t/Bridging.h -Xcc -std=c++23 \
// RUN:   -verify-additional-file %t/Inputs/CxxModule/Unreachable.h

//--- Inputs/CxxModule/module.modulemap
module CxxModule {
  umbrella "."
  requires cplusplus

  // Each header becomes its own auto-generated submodule
  explicit module * { export * }

  export *
}

//--- Inputs/CxxModule/ProvidesBool.h
#pragma once

// Provides operator bool, which triggers some automatic conformances.
struct ProvidesBool { operator bool() const; };

//--- Inputs/CxxModule/Unreachable.h
#pragma once

// This header is NEVER #included on the visible bridging path, so none of the
// definitions here are reachable.

#include <CxxModule/ProvidesBool.h>

// Should not see this error:
//
//     cannot convert 'const Unreachable' to 'bool' without a conversion operator
//
struct Unreachable : ProvidesBool {};

//--- Inputs/CxxModule/Used.h
#pragma once

#include <CxxModule/ProvidesBool.h>

// Only a forward declaration; the full definition is not reachable from here.
struct Unreachable;

struct Used { Unreachable *makeUnreachable() const; };

//--- Bridging.h
// Imports Used.h (and, transitively, ProvidesBool.h) but NOT Unreachable.h
#include <CxxModule/Used.h>

//--- test.swift

// Referencing a Used member whose return type is Unreachable imports
// Unreachable's complete (but unreachable) definition.
func unrelated(_ s: Used) { _ = s.makeUnreachable() }
