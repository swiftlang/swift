// RUN: %target-swift-frontend -parse -verify %s

func memberwiseInit() {} // dummy symbol
func stringify() {} // dummy symbol

extension String {
  static let bar = #stringify(4)

  #memberwiseInit

  #memberwiseInit { "abc" }

  #memberwiseInit(flavor: .chocolate)

  #memberwiseInit(flavor: .chocolate, haha: true)

  #memberwiseInit(flavor: .chocolate, haha: true) { "abc" }

  @available(macOS 999, *)
  public #memberwiseInit()

  static internal #memberwiseInit

  struct Foo {
    #memberwiseInit

    // expected-cparser-error @+1 {{expected a macro identifier for a pound literal declaration}} expected-astgen-error@+1 {{expected identifier in macro expansion}}
    #- // expected-astgen-error {{unexpected code '-' in struct}}
  }

  // expected-cparser-error @+1 {{expected a macro identifier for a pound literal declaration}} expected-astgen-error@+1 {{expected identifier in macro expansion}}
  #()
}

@RandomAttr #someFunc

public #someFunc

#someFunc

func test() {
  @discardableResult #someFunc

  dynamic #someFunc

  @CustomAttr
  #someFunc
}

public # someFunc // expected-cparser-error {{extraneous whitespace between '#' and macro name is not permitted}} {{9-10=}} expected-astgen-error {{extraneous whitespace after '#' is not permitted}}

struct S {
  # someFunc // expected-cparser-error {{extraneous whitespace between '#' and macro name is not permitted}} {{4-5=}} expected-astgen-error {{extraneous whitespace after '#' is not permitted}}
  #class
  # struct Inner {} // expected-cparser-error {{expected a macro identifier for a pound literal declaration}} expected-astgen-error {{found an unexpected second identifier in macro expansion; is there an accidental break?}} expected-cparser-error {{consecutive declarations on a line}} expected-astgen-error {{extraneous whitespace after '#' is not permitted}} expected-astgen-note {{join the identifiers together}}
}
