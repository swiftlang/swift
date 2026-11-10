// RUN: %target-typecheck-verify-swift

struct FooStructConstructorA {
  init // expected-cparser-error {{expected '('}} expected-astgen-error {{expected parameter clause in function signature}}
  // expected-error@-1{{initializer requires a body}}
}

struct FooStructConstructorB {
  init() // expected-error {{initializer requires a body}}
}

struct FooStructConstructorC {
  init {} // expected-cparser-error {{expected '('}}{{7-7=()}} expected-astgen-error {{expected parameter clause in function signature}}
  // expected-note@-1{{'init()' previously declared here}}
  init<T> {} // expected-cparser-error {{expected '('}} {{10-10=()}} expected-astgen-error {{expected parameter clause in function signature}}
  // expected-error@-1{{generic parameter 'T' is not used in function signature}}
  init? { self.init() } // expected-cparser-error {{expected '('}} {{8-8=()}} expected-astgen-error {{expected parameter clause in function signature}}
  // expected-error@-1{{invalid redeclaration of 'init()'}}
}

struct FooStructConstructorD {
  init() -> FooStructConstructorD { }
  // expected-error@-1{{initializers cannot have a result type}}
}

struct FooStructDeinitializerA {
  deinit // expected-cparser-error {{expected '{' for deinitializer}} expected-astgen-error {{expected '{' in body of function declaration}} expected-astgen-note {{'deinit' previously declared here}} expected-astgen-note {{'deinit' previously declared here}}
  deinit x // expected-cparser-error {{deinitializers cannot have a name}} {{10-12=}} expected-astgen-error {{deinitializers cannot have a name}}  expected-cparser-error {{expected '{' for deinitializer}} expected-astgen-error {{invalid redeclaration of 'deinit'}}
  deinit x() // expected-error {{deinitializers cannot have a name}} {{10-11=}} expected-cparser-error {{no parameter clause allowed on deinitializer}} {{11-13=}} expected-cparser-error {{expected '{' for deinitializer}} expected-astgen-error {{invalid redeclaration of 'deinit'}}
}

struct FooStructDeinitializerB { // expected-astgen-note {{consider adding '~Copyable' to struct 'FooStructDeinitializerB'}}
  deinit // expected-cparser-error {{expected '{' for deinitializer}} expected-astgen-error {{deinitializer cannot be declared in struct 'FooStructDeinitializerB' that conforms to 'Copyable'}} expected-astgen-error {{expected '{' in body of function declaration}}
}

struct FooStructDeinitializerC { // expected-note {{consider adding '~Copyable' to struct 'FooStructDeinitializerC'}}
  deinit {} // expected-error {{deinitializer cannot be declared in struct 'FooStructDeinitializerC' that conforms to 'Copyable'}}
}

class FooClassDeinitializerA {
  deinit(a : Int) {} // expected-cparser-error{{no parameter clause allowed on deinitializer}}{{9-18=}} expected-astgen-error {{deinitializers cannot have parameters}}
}

class FooClassDeinitializerB {
  deinit { }
}

class FooClassDeinitializerC {
  deinit x (a : Int) {} // expected-error {{deinitializers cannot have a name}} {{10-12=}} expected-cparser-error{{no parameter clause allowed on deinitializer}}{{12-22=}}
}

init {} // expected-error {{initializers may only be declared within a type}} expected-cparser-error {{expected '('}} {{5-5=()}} expected-astgen-error {{expected parameter clause in function signature}}
init() // expected-error {{initializers may only be declared within a type}}
init() {} // expected-error {{initializers may only be declared within a type}}

deinit {} // expected-error {{deinitializers may only be declared within a class, actor, or non-'Copyable' type}}
deinit // expected-cparser-error {{expected '{' for deinitializer}} expected-astgen-error {{deinitializers may only be declared within a class, actor, or non-'Copyable' type}}
deinit {} // expected-error {{deinitializers may only be declared within a class, actor, or non-'Copyable' type}}

struct BarStruct {
  init() {}
  deinit {} // NOTE: this doesn't get diagnosed with the other errors in this file for some reason.
}

extension BarStruct {
  init(x : Int) {}

  // When/if we allow 'var' in extensions, then we should also allow dtors
  deinit {} // expected-error {{deinitializers may only be declared within a class, actor, or non-'Copyable' type}}
}

enum BarUnion {
  init() {}
  deinit {} // NOTE: this doesn't get diagnosed with the other errors in this file for some reason.
}

extension BarUnion {
  init(x : Int) {}
  deinit {} // expected-error {{deinitializers may only be declared within a class, actor, or non-'Copyable' type}}
}

class BarClass {
  init() {}
  deinit {}
}

extension BarClass {
  convenience init(x : Int) { self.init() }
  deinit {} // expected-error {{deinitializers may only be declared within a class, actor, or non-'Copyable' type}}
}

protocol BarProtocol {
  init() {} // expected-error {{protocol initializers must not have bodies}}
  deinit {} // expected-error {{deinitializers may only be declared within a class, actor, or non-'Copyable' type}}
}

extension BarProtocol {
  init(x : Int) {}
  deinit {} // expected-error {{deinitializers may only be declared within a class, actor, or non-'Copyable' type}}
}

func fooFunc() {
  init() {} // expected-error {{initializers may only be declared within a type}}
  deinit {} // expected-error {{deinitializers may only be declared within a class, actor, or non-'Copyable' type}}
}

func barFunc() {
  var x : () = { () -> () in
    // expected-warning@-1 {{variable 'x' was never used; consider replacing with '_' or removing it}}
    init() {} // expected-error {{initializers may only be declared within a type}}
    return
  } ()

  var y : () = { () -> () in
    // expected-warning@-1 {{variable 'y' was never used; consider replacing with '_' or removing it}}
    deinit {} // expected-error {{deinitializers may only be declared within a class, actor, or non-'Copyable' type}}
    return
  } ()
}
