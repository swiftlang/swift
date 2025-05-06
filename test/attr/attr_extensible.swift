// RUN: %target-typecheck-verify-swift -enable-experimental-feature ExtensibleAttribute

// REQUIRES: swift_feature_ExtensibleAttribute

@extensible
public enum E1 { // Ok
}

@extensible // expected-error {{'@extensible' attribute can only be applied to public or package declarations, but 'E2' is fileprivate}}
fileprivate enum E2 {}

@extensible // expected-error {{cannot use '@extensible' together with '@frozen'}}
@frozen
public enum E3 {
}

@extensible  // expected-error {{'@extensible' attribute can only be applied to public or package declarations, but 'E4' is internal}}
@usableFromInline
enum E4 {}

@extensible // expected-error {{@extensible may only be used on 'enum' declarations}}
struct Test {
  @extensible // expected-error {{@extensible may only be used on 'enum' declarations}}
  var v: Int {
    @extensible // expected-error {{@extensible may only be used on 'enum' declarations}}
    get { 0 }
  }

  @extensible // expected-error {{@extensible may only be used on 'enum' declarations}}
  var v2: String = ""
  
  @extensible // expected-error {{@extensible may only be used on 'enum' declarations}}
  func test() {}

  @extensible // expected-error {{@extensible may only be used on 'enum' declarations}}
  subscript(a: Int) -> Bool {
    get { false }
    set { }
  }
}

@preEnumExtensibility
@extensible
public enum PE {
}

@preEnumExtensibility // expected-error {{@preEnumExtensibility can only be used together with '@extensible' attribute}}
public enum WrongPreE {
}
