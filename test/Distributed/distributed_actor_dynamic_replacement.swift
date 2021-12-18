// RUN: %target-swift-frontend -disable-availability-checking -enable-experimental-distributed -c -enable-batch-mode -module-name foo -primary-file %S/Inputs/dynamic_replacement_da_extension.swift -primary-file %S/Inputs/dynamic_replacement_da_decl.swift

// REQUIRES: concurrency
// REQUIRES: distributed

import _Distributed

func runtimeReceive(da: DA) async throws {
  try await da.hello(other:da)
}
