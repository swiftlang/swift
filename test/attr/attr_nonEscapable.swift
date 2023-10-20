// RUN: %target-typecheck-verify-swift -enable-experimental-feature NonEscapableTypes

// REQUIRES: asserts

@_nonEscapable public struct NES {
  let x: Int

  @_unsafeNonEscapableResult
  init() {
    x = 0
  }

  @_unsafeNonEscapableResult
  static func makeS() -> NES {
    return NES()
  }
}
