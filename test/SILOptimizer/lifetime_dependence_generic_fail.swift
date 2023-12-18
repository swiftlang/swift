// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -o /dev/null \
// RUN:   -verify \
// RUN:   -sil-verify-all \
// RUN:   -module-name test \
// RUN:   -disable-experimental-parser-round-trip \
// RUN:   -enable-experimental-feature NonescapableTypes \
// RUN:   -enable-experimental-feature NoncopyableGenerics \
// RUN:   -Xllvm -enable-lifetime-dependence-diagnostics \
// RUN:   -parse-stdlib -module-name Swift

// REQUIRES: swift_in_compiler

@_marker public protocol Escapable {}

protocol P {
  associatedtype E: ~Escapable
  borrowing func getE() -> _borrow(self) E
}

extension P {
  borrowing func getDefault() -> _borrow(self) E {
    return getE()
  }
}

public struct View: ~Escapable {}

public struct PView: P {
  borrowing func getE() -> _borrow(self) View { return View() }
}

public func pview_ret_concrete(pview: consuming PView) -> _consume(pview) View {
  return pview.getDefault() // expected-error {{lifetime-dependent value escapes its scope}}
  // expected-note @-1 {{it depends on this scoped access to variable 'pview'}}
  // expected-note @-2 {{this use causes the lifetime-dependent value to escape}}
}
