// RUN: %target-typecheck-verify-swift

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
