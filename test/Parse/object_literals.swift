// RUN: %target-typecheck-verify-swift

let _ = #notAPound // expected-error {{no macro named 'notAPound'}}
let _ = #notAPound(1, 2) // expected-error {{no macro named 'notAPound'}}
let _ = #Color // expected-error {{no macro named 'Color'}}

let _ = [##] // expected-cparser-error {{expected a macro identifier}} {{none}} expected-astgen-error {{expected identifier in macro expansion}} expected-astgen-error {{expected ',' in array element}} expected-astgen-error {{expected identifier in macro expansion}}
