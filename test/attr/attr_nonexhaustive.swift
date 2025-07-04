// RUN: %target-typecheck-verify-swift -enable-experimental-feature NonexhaustiveAttribute

// REQUIRES: swift_feature_NonexhaustiveAttribute

@nonexhaustive
public enum E1 { // Ok
}

@nonexhaustive // expected-error {{'@nonexhaustive' attribute can only be applied to public or package declarations, but 'E2' is fileprivate}}
fileprivate enum E2 {}

@nonexhaustive // expected-error {{cannot use '@nonexhaustive' together with '@frozen'}}
@frozen
public enum E3 {
}

@nonexhaustive  // expected-error {{'@nonexhaustive' attribute can only be applied to public or package declarations, but 'E4' is internal}}
@usableFromInline
enum E4 {}

@nonexhaustive // expected-error {{@nonexhaustive may only be used on 'enum' declarations}}
struct Test {
  @nonexhaustive // expected-error {{@nonexhaustive may only be used on 'enum' declarations}}
  var v: Int {
    @nonexhaustive // expected-error {{@nonexhaustive may only be used on 'enum' declarations}}
    get { 0 }
  }

  @nonexhaustive // expected-error {{@nonexhaustive may only be used on 'enum' declarations}}
  var v2: String = ""
  
  @nonexhaustive // expected-error {{@nonexhaustive may only be used on 'enum' declarations}}
  func test() {}

  @nonexhaustive // expected-error {{@nonexhaustive may only be used on 'enum' declarations}}
  subscript(a: Int) -> Bool {
    get { false }
    set { }
  }
}

@nonexhaustive(warn)
public enum PE {
}

@nonexhaustive // expected-note {{attribute already specified here}}
@nonexhaustive(warn) // expected-error {{duplicate attribute}}
public enum WrongPreE {
}
