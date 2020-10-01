// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype V
}

struct Person {
  var name: String = ""
}

struct Row : P {
   typealias V = String
   init(_: V) {}
}

func foo<C: Collection, R: P>(_ collection: C, _: (C.Element.V) -> R) where C.Element: P { }
// expected-note@-1 {{where 'C.Element' = 'Person'}}

func bar(_ arr: [Person]) {
  foo(arr) { person in // expected-error {{global function 'foo' requires that 'Person' conform to 'P'}}
    Row(person.name)
  }
}

