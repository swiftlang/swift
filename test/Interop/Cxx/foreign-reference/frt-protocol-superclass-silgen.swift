// RUN: %target-swift-frontend -emit-silgen -I %S/Inputs -cxx-interoperability-mode=default -disable-availability-checking %s -o %t.sil
// RUN: %FileCheck %s --check-prefix=IMPORTED-SHARED --input-file=%t.sil
// RUN: %FileCheck %s --check-prefix=SWIFTEXT-SHARED --input-file=%t.sil
// RUN: %FileCheck %s --check-prefix=IMPORTED-IMMORT --input-file=%t.sil
// RUN: %FileCheck %s --check-prefix=SWIFTEXT-IMMORT --input-file=%t.sil

import FRTProtocolSuperclass

// MARK: shared FRT witness is the imported C++ method

protocol SharedImportedWitness: SharedBase {
  func tag() -> CInt
}

extension SharedBase: SharedImportedWitness {}

func useSharedImported<T: SharedImportedWitness>(_ obj: T) -> CInt { obj.tag() }

// The imported entry point is lowered with a foreign convention, so its own
// signature is not asserted here. What matters is that the witness table is
// emitted and passes verification.

// IMPORTED-SHARED-LABEL: sil_witness_table{{.*}}SharedBase: SharedImportedWitness
// IMPORTED-SHARED:         method #SharedImportedWitness.tag

// MARK: shared FRT witness is defined in a Swift extension

protocol SharedSwiftWitness: SharedBase {
  func label() -> CInt
}

extension SharedBase: SharedSwiftWitness {
  func label() -> CInt { tag() }
}

func useSharedSwift<T: SharedSwiftWitness>(_ obj: T) -> CInt { obj.label() }

// This one is a native Swift signature, so `self` must be `@guaranteed`.

// SWIFTEXT-SHARED-LABEL: sil hidden {{.*}}[ossa] @{{.*}}5label{{.*}} : $@convention(method) (@guaranteed SharedBase) -> Int32
// SWIFTEXT-SHARED-LABEL: sil_witness_table{{.*}}SharedBase: SharedSwiftWitness
// SWIFTEXT-SHARED:         method #SharedSwiftWitness.label

// MARK: immortal FRT witness is the imported C++ method

protocol ImmortalImportedWitness: ImmortalBase {
  func tag() -> CInt
}

extension ImmortalBase: ImmortalImportedWitness {}

func useImmortalImported<T: ImmortalImportedWitness>(_ obj: T) -> CInt { obj.tag() }

// The type parameter is lowered trivially, so unowned rather than @guaranteed
// IMPORTED-IMMORT-LABEL: sil hidden {{.*}}[ossa] @{{.*}}useImmortalImported{{.*}} : $@convention(thin) <T where T : ImmortalImportedWitness> (T) -> Int32

// The imported entry point is lowered with a foreign convention, so its own
// signature is not asserted here. What matters is that the witness table is
// emitted and passes verification.

// IMPORTED-IMMORT-LABEL: sil_witness_table{{.*}}ImmortalBase: ImmortalImportedWitness
// IMPORTED-IMMORT:         method #ImmortalImportedWitness.tag

// MARK: immortal FRT witness is defined in a Swift extension

protocol ImmortalSwiftWitness: ImmortalBase {
  func label() -> CInt
}

extension ImmortalBase: ImmortalSwiftWitness {
  func label() -> CInt { tag() }
}

func useImmortalSwift<T: ImmortalSwiftWitness>(_ obj: T) -> CInt { obj.label() }

// The concrete witness is trivially lowered too, so both sides say unowned.
// SWIFTEXT-IMMORT-LABEL: sil hidden {{.*}}[ossa] @{{.*}}5label{{.*}} : $@convention(method) (ImmortalBase) -> Int32

// SWIFTEXT-IMMORT-LABEL: sil_witness_table{{.*}}ImmortalBase: ImmortalSwiftWitness
// SWIFTEXT-IMMORT:         method #ImmortalSwiftWitness.label
