// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -dump-source-file-imports -emit-module -plugin-path %swift-plugin-dir -o %t/ClangIncludesExplicit.swiftmodule -I %t/Inputs -enable-experimental-feature SafeInteropWrappers %t/test.swift 2>&1 | %FileCheck %s --check-prefix DUMP
// RUN: %target-swift-frontend -dump-source-file-imports -emit-module -plugin-path %swift-plugin-dir -o %t/ClangIncludesExplicit.swiftmodule -I %t/Inputs -enable-experimental-feature SafeInteropWrappers %t/test.swift -cxx-interoperability-mode=default 2>&1 | %FileCheck %s --check-prefix DUMP --check-prefix DUMP-CXX

// RUN: %target-swift-ide-test -print-module -module-print-hidden -module-to-print=A1 -plugin-path %swift-plugin-dir -I %t -source-filename=x -enable-experimental-feature SafeInteropWrappers | %FileCheck %s --check-prefix CHECK-A1 --implicit-check-not=import --match-full-lines
// RUN: %target-swift-ide-test -print-module -module-print-hidden -module-to-print=A1.B1 -plugin-path %swift-plugin-dir -I %t -source-filename=x -enable-experimental-feature SafeInteropWrappers | %FileCheck %s --check-prefix CHECK-B1 --implicit-check-not=import --match-full-lines
// RUN: %target-swift-ide-test -print-module -module-print-hidden -module-to-print=A1.B1.C1 -plugin-path %swift-plugin-dir -I %t -source-filename=x -enable-experimental-feature SafeInteropWrappers | %FileCheck %s --check-prefix CHECK-C1 --implicit-check-not=import --match-full-lines
// RUN: %target-swift-ide-test -print-module -module-print-hidden -module-to-print=A1.B1.C1.D1 -plugin-path %swift-plugin-dir -I %t -source-filename=x -enable-experimental-feature SafeInteropWrappers | %FileCheck %s --check-prefix CHECK-D1 --implicit-check-not=import --match-full-lines
// RUN: %target-swift-ide-test -print-module -module-print-hidden -module-to-print=A1.E1 -plugin-path %swift-plugin-dir -I %t -source-filename=x -enable-experimental-feature SafeInteropWrappers | %FileCheck %s --check-prefix CHECK-E1 --implicit-check-not=import --match-full-lines
// RUN: %target-swift-ide-test -print-module -module-print-hidden -module-to-print=A1.E1.F1 -plugin-path %swift-plugin-dir -I %t -source-filename=x -enable-experimental-feature SafeInteropWrappers | %FileCheck %s --check-prefix CHECK-F1 --implicit-check-not=import --match-full-lines
// RUN: %target-swift-ide-test -print-module -module-print-hidden -module-to-print=A1.E1.G1 -plugin-path %swift-plugin-dir -I %t -source-filename=x -enable-experimental-feature SafeInteropWrappers | %FileCheck %s --check-prefix CHECK-G1 --implicit-check-not=import --match-full-lines

// RUN: %target-swift-ide-test -print-module -module-print-hidden -module-to-print=A2.B2 -plugin-path %swift-plugin-dir -I %t -source-filename=x -enable-experimental-feature SafeInteropWrappers | %FileCheck %s --check-prefix CHECK-B2 --implicit-check-not=import --match-full-lines
// RUN: %target-swift-ide-test -print-module -module-print-hidden -module-to-print=A2.B2.C2 -plugin-path %swift-plugin-dir -I %t -source-filename=x -enable-experimental-feature SafeInteropWrappers | %FileCheck %s --check-prefix CHECK-C2 --implicit-check-not=import --match-full-lines
// RUN: %target-swift-ide-test -print-module -module-print-hidden -module-to-print=A2.B2.C2.D2 -plugin-path %swift-plugin-dir -I %t -source-filename=x -enable-experimental-feature SafeInteropWrappers | %FileCheck %s --check-prefix CHECK-D2 --implicit-check-not=import --match-full-lines

//--- test.swift
import A1
import A1.B1
import A1.B1.C1
import A1.B1.C1.D1
import A1.E1

import A2.B2
import A2.B2.C2

// DUMP: imports for {{.*}}/test.swift:
// DUMP-NEXT:   Swift
// DUMP-CXX-NEXT:   CxxShim
// DUMP-CXX-NEXT:   Cxx
// DUMP-NEXT:   _StringProcessing
// DUMP-NEXT:   _SwiftConcurrencyShims
// DUMP-NEXT:   _Concurrency
// DUMP-NEXT:   SwiftOnoneSupport
// DUMP-NEXT:   A1
// DUMP-NEXT:   B1
// DUMP-NEXT:   C1
// DUMP-NEXT:   D1
// DUMP-NEXT:   E1
// DUMP-NEXT:   B2
// DUMP-NEXT:   A2
// DUMP-NEXT:   C2

public func callUnsafe(_ p: UnsafeMutableRawPointer) {
  let _ = a1(p, 13)
  let _ = b1(p, 13)
  let _ = c1(p, 13)
  let _ = d1(p, 13)
  let _ = e1_1(p, 13)
  let _ = e1_2(p, 13)
  let _ = b2_1(p, 13)
  let _ = b2_2(p, 13)
  let _ = c2(p, 13)
  let _ = d2(p, 13)
}

public func callSafe(_ p: UnsafeMutableRawBufferPointer) {
  let _ = a1(p)
  let _ = b1(p)
  let _ = c1(p)
  let _ = d1(p)
  let _ = e1_1(p)
  let _ = e1_2(p)
  let _ = b2_1(p)
  let _ = b2_2(p)
  let _ = c2(p)
  let _ = d2(p)
}

//--- Inputs/module.modulemap
module A1 {
  header "A1.h"
  explicit module B1 {
    header "B1.h"
    module C1 {
      header "C1.h"
      export D1
      explicit module D1 {
        header "D1.h"
      }
    }
  }
  module E1 {
    header "E1.h"

    // F1 and G1 are not imported directly from Swift - G1 is implicitly
    // reexported by E1, but F1 ust be implicitly loaded for its decl to be visible
    explicit module F1 {
      header "F1.h"
    }
    module G1 {
      header "G1.h"
    }
  }
}
module A2 {
  module B2 {
    header "B2.h"

    explicit module C2 {
      header "C2.h"

      module D2 {
        header "D2.h"

        module E2 {
          header "E2.h"
        }
      }
    }
  }
}

//--- Inputs/A1.h
#pragma once

#include "E1.h"
#include "B1.h"

// CHECK-A1: import A1.B1
// CHECK-A1: @_exported import A1.E1

f1_t  a1(void * _Nonnull __sized_by(size), int size);

// DUMP-NEXT: imports for A1.a1:
// DUMP-NEXT: imports for @__swiftmacro_So2a115_SwiftifyImportfMp_.swift:
// DUMP-NEXT:   Swift
// DUMP-NEXT:   D1
// DUMP-NEXT:   B1
// DUMP-NEXT:   F1
// DUMP-CXX-NEXT:   CxxShim
// DUMP-CXX-NEXT:   Cxx
// DUMP-NEXT:   _StringProcessing
// DUMP-NEXT:   _SwiftConcurrencyShims
// DUMP-NEXT:   _Concurrency
// DUMP-NEXT:   SwiftOnoneSupport

//--- Inputs/B1.h
#pragma once

#include "A1.h"
#include "F1.h"
#include "C1.h"

// CHECK-B1: import A1
// CHECK-B1: @_exported import A1.B1.C1

c1_t  b1(void * _Nonnull __sized_by(size), int size);

// DUMP-NEXT: imports for A1.b1:
// DUMP-NEXT: imports for @__swiftmacro_So2b115_SwiftifyImportfMp_.swift:
// DUMP-NEXT:   Swift
// DUMP-NEXT:   D1
// DUMP-NEXT:   B1
// DUMP-NEXT:   F1
// DUMP-CXX-NEXT:   CxxShim
// DUMP-CXX-NEXT:   Cxx
// DUMP-NEXT:   _StringProcessing
// DUMP-NEXT:   _SwiftConcurrencyShims
// DUMP-NEXT:   _Concurrency
// DUMP-NEXT:   SwiftOnoneSupport

//--- Inputs/C1.h
#pragma once

typedef int c1_t;
#include "D1.h"

// CHECK-C1: @_exported import A1.B1.C1.D1

f1_t  c1(void * _Nonnull __sized_by(size), int size);

// DUMP-NEXT: imports for A1.c1:
// DUMP-NEXT: imports for @__swiftmacro_So2c115_SwiftifyImportfMp_.swift:
// DUMP-NEXT:   Swift
// DUMP-NEXT:   D1
// DUMP-NEXT:   B1
// DUMP-NEXT:   F1
// DUMP-CXX-NEXT:   CxxShim
// DUMP-CXX-NEXT:   Cxx
// DUMP-NEXT:   _StringProcessing
// DUMP-NEXT:   _SwiftConcurrencyShims
// DUMP-NEXT:   _Concurrency
// DUMP-NEXT:   SwiftOnoneSupport

//--- Inputs/D1.h
#pragma once

#include "F1.h"

// CHECK-D1-NOT: import

f1_t d1(void * _Nonnull __sized_by(size), int size);

// DUMP-NEXT: imports for A1.d1:
// DUMP-NEXT: imports for @__swiftmacro_So2d115_SwiftifyImportfMp_.swift:
// DUMP-NEXT:   Swift
// DUMP-NEXT:   D1
// DUMP-NEXT:   B1
// DUMP-NEXT:   F1
// DUMP-CXX-NEXT:   CxxShim
// DUMP-CXX-NEXT:   Cxx
// DUMP-NEXT:   _StringProcessing
// DUMP-NEXT:   _SwiftConcurrencyShims
// DUMP-NEXT:   _Concurrency
// DUMP-NEXT:   SwiftOnoneSupport

//--- Inputs/E1.h
#pragma once

#include "A1.h"
#include "B1.h"
#include "C1.h"
#include "D1.h"
#include "G1.h"

// CHECK-E1: import A1
// CHECK-E1: import A1.E1.F1
// CHECK-E1: @_exported import A1.E1.G1

f1_t e1_1(void * _Nonnull __sized_by(size), int size);
g1_t e1_2(void * _Nonnull __sized_by(size), int size);

// DUMP-NEXT: imports for A1.e1_1:
// DUMP-NEXT: imports for @__swiftmacro_So4e1_115_SwiftifyImportfMp_.swift:
// DUMP-NEXT:   Swift
// DUMP-NEXT:   D1
// DUMP-NEXT:   B1
// DUMP-NEXT:   F1
// DUMP-CXX-NEXT:   CxxShim
// DUMP-CXX-NEXT:   Cxx
// DUMP-NEXT:   _StringProcessing
// DUMP-NEXT:   _SwiftConcurrencyShims
// DUMP-NEXT:   _Concurrency
// DUMP-NEXT:   SwiftOnoneSupport

// DUMP-NEXT: imports for A1.e1_2:
// DUMP-NEXT: imports for @__swiftmacro_So4e1_215_SwiftifyImportfMp_.swift:
// DUMP-NEXT:   Swift
// DUMP-NEXT:   D1
// DUMP-NEXT:   B1
// DUMP-NEXT:   F1
// DUMP-CXX-NEXT:   CxxShim
// DUMP-CXX-NEXT:   Cxx
// DUMP-NEXT:   _StringProcessing
// DUMP-NEXT:   _SwiftConcurrencyShims
// DUMP-NEXT:   _Concurrency
// DUMP-NEXT:   SwiftOnoneSupport

//--- Inputs/F1.h
#pragma once

// CHECK-F1-NOT: import

#define __sized_by(s) __attribute__((__sized_by__(s)))

typedef int f1_t;

//--- Inputs/G1.h
#pragma once

// CHECK-G1-NOT: import

typedef int g1_t;

//--- Inputs/B2.h
#pragma once

#include "A1.h"
#include "B1.h"
#include "C1.h"
#include "C2.h"
#include "D1.h"

// CHECK-B2: import A1
// CHECK-B2: import A2.B2.C2


c1_t b2_1(void * _Nonnull __sized_by(size), int size);
c2_t b2_2(void * _Nonnull __sized_by(size), int size);

// DUMP-NEXT: imports for A2.b2_1:
// DUMP-NEXT: imports for @__swiftmacro_So4b2_115_SwiftifyImportfMp_.swift:
// DUMP-NEXT:   Swift
// DUMP-NEXT:   C2
// DUMP-NEXT:   D1
// DUMP-NEXT:   A1
// DUMP-NEXT:   C1
// DUMP-NEXT:   B1
// DUMP-CXX-NEXT:   CxxShim
// DUMP-CXX-NEXT:   Cxx
// DUMP-NEXT:   _StringProcessing
// DUMP-NEXT:   _SwiftConcurrencyShims
// DUMP-NEXT:   _Concurrency
// DUMP-NEXT:   SwiftOnoneSupport

// DUMP-NEXT: imports for A2.b2_2:
// DUMP-NEXT: imports for @__swiftmacro_So4b2_215_SwiftifyImportfMp_.swift:
// DUMP-NEXT:   Swift
// DUMP-NEXT:   C2
// DUMP-NEXT:   D1
// DUMP-NEXT:   A1
// DUMP-NEXT:   C1
// DUMP-NEXT:   B1
// DUMP-CXX-NEXT:   CxxShim
// DUMP-CXX-NEXT:   Cxx
// DUMP-NEXT:   _StringProcessing
// DUMP-NEXT:   _SwiftConcurrencyShims
// DUMP-NEXT:   _Concurrency
// DUMP-NEXT:   SwiftOnoneSupport

//--- Inputs/C2.h
#pragma once

#include "D2.h"
#include "B1.h"
#include "C1.h"

// CHECK-C2: @_exported import A2.B2.C2.D2

typedef int c2_t;

e2_t c2(void * _Nonnull __sized_by(size), int size);

// DUMP-NEXT: imports for A2.c2:
// DUMP-NEXT: imports for @__swiftmacro_So2c215_SwiftifyImportfMp_.swift:
// DUMP-NEXT:   Swift
// DUMP-NEXT:   C2
// DUMP-NEXT:   D1
// DUMP-NEXT:   A1
// DUMP-NEXT:   C1
// DUMP-NEXT:   B1
// DUMP-CXX-NEXT:   CxxShim
// DUMP-CXX-NEXT:   Cxx
// DUMP-NEXT:   _StringProcessing
// DUMP-NEXT:   _SwiftConcurrencyShims
// DUMP-NEXT:   _Concurrency
// DUMP-NEXT:   SwiftOnoneSupport

//--- Inputs/D2.h
#pragma once

#include "E2.h"

// CHECK-D2: @_exported import A2.B2.C2.D2.E2

e2_t d2(void * _Nonnull __sized_by(size), int size);

// DUMP-NEXT: imports for A2.d2:
// DUMP-NEXT: imports for @__swiftmacro_So2d215_SwiftifyImportfMp_.swift:
// DUMP-NEXT:   Swift
// DUMP-NEXT:   C2
// DUMP-NEXT:   D1
// DUMP-NEXT:   A1
// DUMP-NEXT:   C1
// DUMP-NEXT:   B1
// DUMP-CXX-NEXT:   CxxShim
// DUMP-CXX-NEXT:   Cxx
// DUMP-NEXT:   _StringProcessing
// DUMP-NEXT:   _SwiftConcurrencyShims
// DUMP-NEXT:   _Concurrency
// DUMP-NEXT:   SwiftOnoneSupport

//--- Inputs/E2.h
#pragma once

#define __sized_by(s) __attribute__((__sized_by__(s)))

typedef int e2_t;

// DUMP-NOT: imports
