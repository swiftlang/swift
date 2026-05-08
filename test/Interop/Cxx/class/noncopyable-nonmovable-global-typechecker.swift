// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default %s -verify-ignore-unrelated

import NoncopyableNonmovableGlobal

func useTokens() {
    let _ = ns.Tokens // expected-error {{type 'ns' has no member 'Tokens'}}
}
