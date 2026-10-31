// RUN: %target-swift-frontend -parse -verify %s

#macro

#macro(1)

#macro(1, 2)

_ = #macro(1, 2, 3)

_ = #trailing {
  1
}

_ = #trailing() {
  1
}

_ = #trailing(x) {
  1
}

_ = #trailing(x, y, z) {
  1
} again: {
  2
} yetAgain: {
  3
}

_ = #another {
  // expected-cparser-error @+1 {{expected a macro identifier}} expected-astgen-error @+1 {{expected identifier in macro expansion}}
  #-
}

// expected-cparser-error @+1 {{expected a macro identifier for a pound literal expression}} expected-astgen-error @+1 {{expected identifier in macro expansion}}
_ = #()

do {
  _ = # // expected-cparser-error {{expected a macro identifier for a pound literal expression}} expected-astgen-error {{expected identifier in macro expansion}}
  name()
}
do {
  _ = # macro() // expected-cparser-error {{extraneous whitespace between '#' and macro name is not permitted}} {{8-9=}} expected-astgen-error {{extraneous whitespace after '#' is not permitted}} {{8-9=}}
}
do {
  _ = #public()
}
do {
  _ = # public() // expected-cparser-error {{expected a macro identifier for a pound literal expression}} expected-astgen-error {{extraneous whitespace after '#' is not permitted}}
}
