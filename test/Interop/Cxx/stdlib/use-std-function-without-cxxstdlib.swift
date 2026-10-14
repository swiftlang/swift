// Initializing a std::function from a Swift closure needs __SwiftFunctionWrapper
// from the CxxStdlibShim module, which only the CxxStdlib overlay imports. If
// the overlay is not loaded, for example because no visible module re-exports
// std, that initializer is not imported. This used to crash in SILGen.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-silgen %t/main.swift -I %t/Inputs -cxx-interoperability-mode=default -verify -verify-additional-prefix without-overlay-
// RUN: %target-swift-frontend -emit-silgen %t/main.swift -I %t/Inputs -cxx-interoperability-mode=default -D IMPORT_CXXSTDLIB | %FileCheck %s

//--- Inputs/module.modulemap
module Callback {
  header "callback.h"
  requires cplusplus
  // No `export *`, so std is not visible to clients of this module.
}

//--- Inputs/callback.h
#pragma once
#include <functional>

inline std::function<void()> makeCallback() { return [] {}; }
inline void invoke(const std::function<void()> &callback) { callback(); }

//--- main.swift
import Callback
#if IMPORT_CXXSTDLIB
import CxxStdlib
#endif

invoke(makeCallback())

var count = 0
invoke(.init({ count += 1 })) // expected-without-overlay-error {{argument passed to call that takes no arguments}}

// CHECK: function_ref __SwiftFunctionWrapper<{{.*}}>.init(closure:)
