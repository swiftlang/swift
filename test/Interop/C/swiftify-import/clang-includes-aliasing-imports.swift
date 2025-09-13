// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -dump-source-file-imports -emit-module -plugin-path %swift-plugin-dir -o %t/ClangIncludesExplicit.swiftmodule -I %t/Inputs -enable-experimental-feature SafeInteropWrappers %t/test.swift 2>&1 | %FileCheck %s --check-prefix DUMP
// RUN: %target-swift-frontend -dump-source-file-imports -emit-module -plugin-path %swift-plugin-dir -o %t/ClangIncludesExplicit.swiftmodule -I %t/Inputs -enable-experimental-feature SafeInteropWrappers %t/test.swift -cxx-interoperability-mode=default 2>&1 | %FileCheck %s --check-prefix DUMP --check-prefix DUMP-CXX

// RUN: %target-swift-ide-test -print-module -module-print-hidden -module-to-print=A1 -plugin-path %swift-plugin-dir -I %t -source-filename=x -enable-experimental-feature SafeInteropWrappers | %FileCheck %s --check-prefix CHECK-A1 --implicit-check-not=import --match-full-lines
// RUN: %target-swift-ide-test -print-module -module-print-hidden -module-to-print=A2 -plugin-path %swift-plugin-dir -I %t -source-filename=x -enable-experimental-feature SafeInteropWrappers | %FileCheck %s --check-prefix CHECK-A2 --implicit-check-not=import --match-full-lines

//--- test.swift
import A1

import A2

// DUMP: imports for {{.*}}/test.swift:
// DUMP-NEXT:   Swift
// DUMP-CXX-NEXT:   CxxShim
// DUMP-CXX-NEXT:   Cxx
// DUMP-NEXT:   _StringProcessing
// DUMP-NEXT:   _SwiftConcurrencyShims
// DUMP-NEXT:   _Concurrency
// DUMP-NEXT:   SwiftOnoneSupport
// DUMP-NEXT:   A1
// DUMP-NEXT:   A2

public func callUnsafe(_ p: UnsafeMutableRawPointer) {
  let _ = a1(p, 13)
  let _ = a2(p, 13)
}

public func callSafe(_ p: UnsafeMutableRawBufferPointer) {
  let _ = a1(p)
  let _ = a2(p)
}

//--- Inputs/module.modulemap
module A1 {
  header "A1.h"
  explicit module Aliasing {
    // Make sure that the full module path is taken into account
    // and not just the module name
    header "Aliasing1.h"
    explicit module Aliasing {
      header "Aliasing2.h"
    }
  }
}
module A2 {
  header "A2.h"
}
module A3 {
  explicit module Aliasing {
    header "Aliasing3.h"
  }
}
module A4 {
  explicit module Aliasing {
    header "Aliasing4.h"
  }
}

//--- Inputs/A1.h
#pragma once

#include "Aliasing1.h"
#include "Aliasing2.h"

// CHECK-A1: import A1.Aliasing
// CHECK-A1: import A1.Aliasing.Aliasing

#define __sized_by(s) __attribute__((__sized_by__(s)))

aliasing1_t a1(void * _Nonnull __sized_by(size), aliasing2_t size);

// DUMP-NEXT: imports for A1.a1:
// DUMP-NEXT: imports for @__swiftmacro_So2a115_SwiftifyImportfMp_.swift:
// DUMP-NEXT:   Swift
// DUMP-NEXT:   Aliasing
// DUMP-NEXT:   Aliasing
// DUMP-CXX-NEXT:   CxxShim
// DUMP-CXX-NEXT:   Cxx
// DUMP-NEXT:   _StringProcessing
// DUMP-NEXT:   _SwiftConcurrencyShims
// DUMP-NEXT:   _Concurrency
// DUMP-NEXT:   SwiftOnoneSupport

//--- Inputs/Aliasing1.h
#pragma once

typedef int aliasing1_t;

//--- Inputs/Aliasing2.h
#pragma once

typedef int aliasing2_t;

//--- Inputs/Aliasing3.h
#pragma once

typedef int aliasing3_t;

//--- Inputs/Aliasing4.h
#pragma once

typedef int aliasing4_t;

//--- Inputs/A2.h
#pragma once

#include "Aliasing3.h"
#include "Aliasing4.h"

// CHECK-A2-NOT: import

#define __sized_by(s) __attribute__((__sized_by__(s)))

aliasing3_t a2(void * _Nonnull __sized_by(size), aliasing4_t size);

// DUMP-NEXT: imports for A2.a2:
// DUMP-NEXT: imports for @__swiftmacro_So2a215_SwiftifyImportfMp_.swift:
// DUMP-NEXT:   Swift
// DUMP-NEXT:   Aliasing
// DUMP-NEXT:   A4
// DUMP-NEXT:   Aliasing
// DUMP-NEXT:   A3
// DUMP-CXX-NEXT:   CxxShim
// DUMP-CXX-NEXT:   Cxx
// DUMP-NEXT:   _StringProcessing
// DUMP-NEXT:   _SwiftConcurrencyShims
// DUMP-NEXT:   _Concurrency
// DUMP-NEXT:   SwiftOnoneSupport

// DUMP-NOT: imports
