// RUN: %target-typecheck-verify-swift -enable-experimental-feature NonescapableTypes

// REQUIRES: asserts

@_nonescapable public struct NES {
  let x: Int

  @_unsafeNonescapableResult
  init() {
    x = 0
  }

  @_unsafeNonescapableResult
  static func makeS() -> NES {
    return NES()
  }
}
