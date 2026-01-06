// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/ClangIncludesExplicit.swiftmodule -I %t/Inputs -enable-experimental-feature SafeInteropWrappers %t/test.swift
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/ClangIncludesExplicit.swiftmodule -I %t/Inputs -enable-experimental-feature SafeInteropWrappers %t/test.swift -cxx-interoperability-mode=default

//--- test.swift
import A1.B1.C1.D1

public func callUnsafe(_ p: UnsafeMutableRawPointer) {
  let _ = foo(p, 13)
}

public func callSafe(_ p: UnsafeMutableRawBufferPointer) {
  let _ = foo(p)
}

//--- Inputs/module.modulemap
module A1 {
  explicit module B1 {
    explicit module C1 {
      explicit module D1 {
        header "D1.h"
      }
    }
  }
}

module A2 {
 explicit module B2 {
    header "B2.h"
    export C2

    explicit module C2 {
      header "C2.h"
    }
  }
}

//--- Inputs/D1.h
#pragma once

#include "B2.h"
#define __sized_by(s) __attribute__((__sized_by__(s)))

c2_t foo(void * _Nonnull __sized_by(size), int size);

//--- Inputs/B2.h
#pragma once

#include "C2.h"

//--- Inputs/C2.h
#pragma once

typedef int c2_t;
