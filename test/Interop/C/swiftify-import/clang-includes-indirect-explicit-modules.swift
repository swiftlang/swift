// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-ide-test -print-module -module-print-hidden -module-to-print=A1.B1 -plugin-path %swift-plugin-dir -I %t -source-filename=x -enable-experimental-feature SafeInteropWrappers | %FileCheck %s --check-prefix CHECK-B1 --implicit-check-not=import --match-full-lines
// RUN: %target-swift-ide-test -print-module -module-print-hidden -module-to-print=A1.B1.C1 -plugin-path %swift-plugin-dir -I %t -source-filename=x -enable-experimental-feature SafeInteropWrappers | %FileCheck %s --check-prefix CHECK-C1 --implicit-check-not=import --match-full-lines

// RUN: %target-swift-ide-test -print-module -module-print-hidden -module-to-print=A2.B2 -plugin-path %swift-plugin-dir -I %t -source-filename=x -enable-experimental-feature SafeInteropWrappers | %FileCheck %s --check-prefix CHECK-B2 --implicit-check-not=import --match-full-lines

//--- test.swift
import A1.B1
import A2.B2

public func callUnsafe(_ p: UnsafeMutableRawPointer) {
  let _ = b1b(p, 13)
  let _ = b1c(p, 13)
  let _ = b1d(p, 13)
  let _ = b2b(p, 13)
  let _ = b2c(p, 13)
  let _ = b2d(p, 13)
}

public func callSafe(_ p: UnsafeMutableRawBufferPointer) {
  let _ = b1b(p)
  let _ = b1c(p)
  let _ = b1d(p)
  let _ = b2b(p)
  let _ = b2c(p)
  let _ = b2d(p)
}
//--- Inputs/module.modulemap
module A1 {
  explicit module B1 {
    header "B1.h"
    explicit module C1 {
      header "C1.h"
      explicit module D1 {
        header "D1.h"
      }
    }
  }
}
module A2 {
  explicit module B2 {
    header "B2.h"
  }
}

//--- Inputs/B1.h
#pragma once

#include "C1.h"
#include "D1.h"

// CHECK-B1: import A1.B1.C1
// CHECK-B1: import A1.B1.C1.D1

typedef int b1_t;

b1_t b1b(void * _Nonnull __sized_by(size), int size);
c1_t b1c(void * _Nonnull __sized_by(size), int size);
d1_t b1d(void * _Nonnull __sized_by(size), int size);

//--- Inputs/C1.h
#pragma once

#include "D1.h"

// CHECK-C1: import A1.B1.C1.D1

typedef int c1_t;


//--- Inputs/D1.h
#pragma once

#define __sized_by(s) __attribute__((__sized_by__(s)))

typedef int d1_t;

//--- Inputs/B2.h
#pragma once

#include "B1.h"
#include "C1.h"
#include "D1.h"

// CHECK-B2-NOT: import

b1_t b2b(void * _Nonnull __sized_by(size), int size);
c1_t b2c(void * _Nonnull __sized_by(size), int size);
d1_t b2d(void * _Nonnull __sized_by(size), int size);
