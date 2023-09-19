// RUN: %target-swift-frontend -module-name test -emit-sil %s -o /dev/null -verify

@_nonEscapable public struct NES {
  public let x: Int64

  @_unsafeNonEscapableResult
  init() {
    x = 0
  }

  @_unsafeNonEscapableResult
  static func makeNES() -> NES {
    return NES()
  }
}

// -----------------------------------------------------------------------------
// NonEscapingScope.orphan

// ### Diagnosing lifetime dependence in $s4test9createNESs5Int64VyF
// ### Diagnosing lifetime dependence on scope: orphan(  %2 = apply %1(%0) : $@convention(method) (@thin NES.Type) -> NES
func createNES() -> Int64 {
  let nes = NES.makeNES()
  return nes.x
}

func returnNES() -> NES {
  return NES.makeNES()
}
