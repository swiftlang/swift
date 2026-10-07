// RUN: %target-swift-frontend -emit-ir -g %s -target %target-swift-5.9-abi-triple

// https://github.com/apple/swift/issues/XXXXX
// IRGenDebugInfo type reconstruction crash with parameter packs and optional
// existential types. The combination of:
// 1. Optional existential type: (any Sendable)?
// 2. Parameter pack with consuming ownership: consuming (repeat each T)
// 3. Both in the same function type
// would cause a crash in getMangledName due to type comparison failure
// during the debug type round-trip verification.

actor Machine<each Input: Sendable> {
  typealias Fn = (
    (any Sendable)?,
    consuming (repeat each Input)
  ) -> Void

  private let fn: Fn

  init(fn: @escaping Fn) {
    self.fn = fn
  }
}
