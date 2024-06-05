// RUN: %target-typecheck-verify-swift -enable-experimental-feature NonescapableTypes

// REQUIRES: asserts

public struct NES : ~Escapable {
  let x: Int

  // TODO: dependsOn(immortal)
  @_unsafeNonescapableResult
  init() {
    x = 0
  }

  // TODO: dependsOn(immortal)
  @_unsafeNonescapableResult
  static func makeS() -> NES {
    return NES()
  }
}

struct BC {
  public var nes: NES {
    // TODO: dependsOn(immortal)
    @_unsafeNonescapableResult
    get {
      NES()
    }
  }
}
