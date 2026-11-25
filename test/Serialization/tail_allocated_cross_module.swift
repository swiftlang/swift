// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -target %target-cpu-apple-macos14 -emit-module %t/Base.swift -o %t/Base.swiftmodule -module-name Base -parse-as-library -enable-experimental-feature Embedded
// RUN: %target-swift-frontend -target %target-cpu-apple-macos14 -emit-module %t/Lib.swift -o %t/Lib.swiftmodule -module-name Lib -parse-as-library -I %t -enable-experimental-feature Embedded -enable-experimental-feature TailAllocatedAttribute
// RUN: %target-swift-frontend -target %target-cpu-apple-macos14 -emit-silgen %t/main.swift -module-name main -I %t -enable-experimental-feature Embedded | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: embedded_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_TailAllocatedAttribute

// A subclass's deinit, SILGen'd in a client module, reads the counts property
// of a deserialized @_tailAllocated attribute. The property is declared in a
// superclass from a third module.

//--- Base.swift
open class CountsBase {
  public final var _tailCount: Int
  public init() { _tailCount = 0 }
}

//--- Lib.swift
import Base

@_tailAllocated(counts: _tailCount, types: Element)
open class Buffer<Element>: CountsBase {}

//--- main.swift
import Lib

final class Client: Buffer<UInt8> {}

// CHECK-LABEL: sil hidden [ossa] @$e4main6ClientCfD :
// CHECK:         ref_element_addr {{%.*}}, #CountsBase._tailCount
// CHECK:         dealloc_ref [tail_elems $UInt8 * {{%.*}}] {{%.*}}
// CHECK-LABEL: } // end sil function '$e4main6ClientCfD'
