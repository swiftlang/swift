import main
import HasOverlay

func test() {
  var a = getShadowedFromClang()
  a = main.shadowedFromClang // okay
  a = HasOverlay.shadowedFromSwift // expected-error {{cannot assign}}

  var b = HasOverlay.shadowedFromSwift
  b = main.shadowedFromSwift // okay
  b = getShadowedFromClang() // expected-error {{cannot assign}}
}