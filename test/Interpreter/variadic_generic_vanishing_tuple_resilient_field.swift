// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(vanishing_tuple_boxes)) -enable-library-evolution %S/Inputs/vanishing_tuple_boxes.swift -emit-module -emit-module-path %t/vanishing_tuple_boxes.swiftmodule -module-name vanishing_tuple_boxes
// RUN: %target-codesign %t/%target-library-name(vanishing_tuple_boxes)
// RUN: %target-build-swift -target %target-swift-5.9-abi-triple %s -lvanishing_tuple_boxes -I %t -L %t -o %t/main %target-rpath(%t)
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main %t/%target-library-name(vanishing_tuple_boxes) | %FileCheck %s

// REQUIRES: executable_test

// Match the swift-5.9 target triple above.
// REQUIRES: stdlib_5_9_runtime

// Storing a pack-expansion tuple property hits an unrelated pre-existing SILGen
// assertion under SIL opaque values (TupleInitialization::copyOrInitValueInto);
// the metadata-completion bug this test guards is in IRGen and is exercised by
// every other configuration.
// XFAIL: swift_test_mode_optimize_none_with_opaque_values


// Instantiating metadata for a variadic-generic struct that has a resilient
// stored property laid out *before* a tuple built from the pack used to crash:
// the resilient field's metadata accessor binds the metadata pack into the
// completion function's local cache at Abstract state, and the subsequent
// vanishing-tuple element lookup then passed the metadata *pack pointer* to
// swift_checkMetadataState as if it were a scalar Metadata*, corrupting the
// element pointer and crashing.

import vanishing_tuple_boxes

struct Repeater<each Input> {
  // Field 0: a resilient generic type parameterized by the nested Storage,
  // laid out before the pack tuple field. Storing it forces a runtime metadata
  // instantiation that binds the metadata pack.
  private var __storage: Box<Storage> = Box(Storage())
  private var input: (repeat each Input)
  init(_ input: repeat each Input) { self.input = (repeat each input) }
}

// The nested type captures the pack, so its accessor is what seeds the pack in
// the completion function's local type-data cache.
extension Repeater { final class Storage {} }

print("before")
// CHECK: before
_ = Repeater(1)
print("after")
// CHECK: after
