// RUN: %target-typecheck-verify-swift

func noEscapeWAE(f: () -> Void) {
  withoutActuallyEscaping(f) { $0() }  
}

func escapingWAE(f: @escaping () -> Void) {
  withoutActuallyEscaping(f) { $0() }  
}

func inoutWAE(f: inout () -> Void) {
  withoutActuallyEscaping(f) { $0() }  
}

func rethrowThroughInoutWAE(f: inout () throws -> Void) throws {
  try withoutActuallyEscaping(f) { try $0() }  
}

func consumingWAE(f: consuming @escaping () -> Void) {
  withoutActuallyEscaping(f) { $0() }  
}

func rethrowThroughConsumingWAE(f: consuming @escaping () throws -> Void) throws {
  try withoutActuallyEscaping(f) { try $0() }  
}

func sendingEscapingAsyncWAE(f: sending @escaping () async -> Void) async {
  await withoutActuallyEscaping(f) { await $0() }  
}

func rethrowThroughSendingEscapingAsyncWAE(f: sending @escaping () async throws -> Void) async throws {
  try await withoutActuallyEscaping(f) { try await $0() }  
}

func sendingNoEscapeAsyncWAE(f: sending () async -> Void) async {
  await withoutActuallyEscaping(f) { await $0() }  
}

func rethrowThroughSendingNoEscapeAsyncWAE(f: sending () async throws -> Void) async throws {
  try await withoutActuallyEscaping(f) { try await $0() }  
}

func passNoEscapeClosureViaVarWAE(f: () -> Void) {
  var x = f
  withoutActuallyEscaping(x) { $0() }
  x = {}
}

func passEscapingClosureViaVarWAE(f: @escaping () -> Void) {
  var x = f
  withoutActuallyEscaping(x) { $0() }
  x = {}
}

func passInoutClosureViaVarWAE(f: inout () -> Void) {
  var x = f
  withoutActuallyEscaping(x) { $0() }
  x = {}
}

func passConsumingClosureViaVarWAE(f: consuming @escaping () -> Void) {
  var x = f
  withoutActuallyEscaping(x) { $0() }
  x = {}
}

func passSendingClosureViaVarWAE(f: consuming @escaping () -> Void) {
  var x = f
  withoutActuallyEscaping(x) { $0() }
  x = {}
}
