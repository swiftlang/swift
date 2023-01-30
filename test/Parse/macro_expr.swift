// RUN: %target-swift-frontend -parse -enable-experimental-feature Macros -verify %s

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
  // expected-error @+1 {{expected a macro identifier for a pound literal expression}}
  #-
}

// expected-error @+1 {{expected a macro identifier for a pound literal expression}}
_ = #()
