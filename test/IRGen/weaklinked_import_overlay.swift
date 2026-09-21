// RUN: %empty-directory(%t)
// RUN: split-file %s %t
//
// Build the Swift overlay for the Clang module 'Wrapper'.
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Wrapper.swiftmodule -module-name Wrapper -parse-as-library %t/Wrapper.swift -I %t/include -enable-library-evolution
//
// Build an intermediate module that re-exports the overlay.
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/Intermediate.swiftmodule -module-name Intermediate -parse-as-library %t/Intermediate.swift -I %t -I %t/include -enable-library-evolution
//
// RUN: %target-swift-frontend -primary-file %t/DirectClient.swift -I %t -I %t/include -emit-ir | %FileCheck %s --check-prefix=DIRECT
// RUN: %target-swift-frontend -primary-file %t/TransitiveClient.swift -I %t -I %t/include -emit-ir | %FileCheck %s --check-prefix=TRANSITIVE

// UNSUPPORTED: OS=windows-msvc

//--- include/module.modulemap

module Wrapper {
  header "Wrapper.h"
  export *

  module WrapperCore {
    header "WrapperCore.h"
    export *
  }
}

module Core {
  header "Core.h"
  export *
  export_as Wrapper
}

module Unrelated {
  header "Unrelated.h"
  export *
}

//--- include/Wrapper.h

#include "WrapperCore.h"

extern void wrapper_fn(void);

//--- include/WrapperCore.h

#include "Core.h"
#include "Unrelated.h"

//--- include/Core.h

extern void core_fn(void);

//--- include/Unrelated.h

extern void unrelated_fn(void);

//--- Wrapper.swift

@_exported import Wrapper

public func overlay_fn() { }

//--- Intermediate.swift

@_exported import Wrapper

//--- DirectClient.swift

@_weakLinked import Wrapper

func testDirect() {
  // DIRECT: declare extern_weak swiftcc void @"$s7Wrapper10overlay_fnyyF"()
  overlay_fn()

  // DIRECT: declare extern_weak void @wrapper_fn()
  wrapper_fn()

  // DIRECT: declare extern_weak void @core_fn()
  core_fn()

  // DIRECT: declare void @unrelated_fn()
  unrelated_fn()
}

//--- TransitiveClient.swift

@_weakLinked import Intermediate

func testTransitive() {
  // TRANSITIVE: declare extern_weak swiftcc void @"$s7Wrapper10overlay_fnyyF"()
  overlay_fn()

  // TRANSITIVE: declare extern_weak void @wrapper_fn()
  wrapper_fn()

  // TRANSITIVE: declare extern_weak void @core_fn()
  core_fn()

  // TRANSITIVE: declare void @unrelated_fn()
  unrelated_fn()
}
