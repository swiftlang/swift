// RUN: %swift -parse %s -verify -parse-as-library

class A {
  func ret_sametype() -> Int { return 0 }
  func ret_subclass() -> A { return self }
  func ret_subclass_rev() -> B { return B() }
  func ret_nonclass_optional() -> Int? { return .None }
  func ret_nonclass_optional_rev() -> Int { return 0 }
  func ret_class_optional() -> B? { return .None }
  func ret_class_optional_rev() -> A { return self }
  func ret_class_uoptional() -> @unchecked B? { return B() }
  func ret_class_uoptional_rev() -> A { return self }
  func ret_class_optional_uoptional() -> B? { return .None }
  func ret_class_optional_uoptional_rev() -> @unchecked A? { return self }

  func param_sametype(x : Int) {}
  func param_subclass(x : B) {}
  func param_subclass_rev(x : A) {}
  func param_nonclass_optional(x : Int) {}
  func param_nonclass_optional_rev(x : Int?) {}
  func param_class_optional(x : B) {}
  func param_class_optional_rev(x : B?) {}
  func param_class_uoptional(x : B) {}
  func param_class_uoptional_rev(x : @unchecked B?) {}
  func param_class_optional_uoptional(x : @unchecked B?) {}
  func param_class_optional_uoptional_rev(x : B?) {}
}

class B : A {
  func ret_sametype() -> Int { return 1 }
  func ret_subclass() -> B { return self }
  func ret_subclass_rev() -> A { return self } // expected-error {{cannot overload a declaration from a superclass}}
  func ret_nonclass_optional() -> Int { return 0 } // expected-error {{cannot overload a declaration from a superclass}}
  func ret_nonclass_optional_rev() -> Int? { return 0 } // expected-error {{cannot overload a declaration from a superclass}}
  func ret_class_optional() -> B { return self }
  func ret_class_optional_rev() -> A? { return self } // expected-error {{cannot overload a declaration from a superclass}}
  func ret_class_uoptional() -> B { return self }
  func ret_class_uoptional_rev() -> @unchecked A? { return self } // expected-error {{cannot overload a declaration from a superclass}}
  func ret_class_optional_uoptional() -> @unchecked B? { return self }
  func ret_class_optional_uoptional_rev() -> A? { return self } // expected-error {{cannot overload a declaration from a superclass}}

  func param_sametype(x : Int) {}
  func param_subclass(x : A) {}
  func param_subclass_rev(x : B) {} // expected-error {{cannot overload a declaration from a superclass}}
  func param_nonclass_optional(x : Int?) {} // expected-error {{cannot overload a declaration from a superclass}}
  func param_nonclass_optional_rev(x : Int) {} // expected-error {{cannot overload a declaration from a superclass}}
  func param_class_optional(x : B?) {}
  func param_class_optional_rev(x : B) {} // expected-error {{cannot overload a declaration from a superclass}}
  func param_class_uoptional(x : @unchecked B?) {}
  func param_class_uoptional_rev(x : B) {} // expected-error {{cannot overload a declaration from a superclass}}
  func param_class_optional_uoptional(x : B?) {}
  func param_class_optional_uoptional_rev(x : @unchecked B?) {} // expected-error {{cannot overload a declaration from a superclass}}
}
