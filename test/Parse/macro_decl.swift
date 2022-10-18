// RUN: %target-swift-frontend -parse -enable-experimental-feature Macros -verify %s

func memberwiseInit() {} // dummy symbol
func stringify() {} // dummy symbol

extension String {
  static let bar = #stringify(4)

  #memberwiseInit

  #memberwiseInit { "abc" }

  #memberwiseInit(flavor: .chocolate)

  #memberwiseInit(flavor: .chocolate, haha: true)

  #memberwiseInit(flavor: .chocolate, haha: true) { "abc" }

  struct Foo {
    #memberwiseInit

    // expected-error @+1 {{expected a macro identifier for a pound literal declaration}}
    #-
  }

  // expected-error @+1 {{expected a macro identifier for a pound literal declaration}}
  #()
}
