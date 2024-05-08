// RUN: %target-typecheck-verify-swift -enable-experimental-feature NonescapableTypes

// REQUIRES: asserts

public struct NES : ~Escapable {
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

struct BC {
  public var nes: NES {
    @_unsafeNonescapableResult
    get {
      NES()
    }
  }
}
