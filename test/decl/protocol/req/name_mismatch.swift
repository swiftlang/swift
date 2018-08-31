// RUN: %target-typecheck-verify-swift

protocol P {
  func foo(_ i: Int, x: Float) // expected-note 5 {{requirement 'foo(_:x:)' declared here}}
}

struct S1 : P {
  func foo(_ i: Int, x: Float) { }
}

struct S2 : P {
  func foo(_ i: Int, _ x: Float) { } // expected-error{{method 'foo' has different argument labels from those required by protocol 'P' ('foo(_:x:)')}}{{22-24=}}
}

struct S3 : P {
  func foo(_ i: Int, y: Float) { } // expected-error{{method 'foo(_:y:)' has different argument labels from those required by protocol 'P' ('foo(_:x:)')}}{{22-22=x }}
}

struct S4 : P {
  func foo(_ i: Int, _: Float) { } // expected-error{{method 'foo' has different argument labels from those required by protocol 'P' ('foo(_:x:)')}}{{22-22=x }}
}

struct S5 : P {
  func foo(_ i: Int, z x: Float) { } // expected-error{{method 'foo(_:z:)' has different argument labels from those required by protocol 'P' ('foo(_:x:)')}}{{22-24=}}
}

struct S5a {
  func foo(_ i: Int, z x: Float) { } // expected-note {{'foo(_:z:)' declared here}} {{none}}
}
extension S5a: P {} // expected-error{{method 'foo(_:z:)' has different argument labels from those required by protocol 'P' ('foo(_:x:)')}} {{none}}

struct Loadable { }

protocol LabeledRequirement {
  func method(x: Loadable)
}

struct UnlabeledWitness : LabeledRequirement {
  func method(x _: Loadable) {}
}

// rdar://problem/21333445
protocol P2 {
	init(_ : Int) // expected-note{{requirement 'init' declared here}}
}

struct XP2 : P2 { // expected-error{{initializer 'init(foo:)' has different argument labels from those required by protocol 'P2' ('init')}}
  let foo: Int 
}

// rdar://problem/22981205
protocol P3 {
  subscript(val: String, label arg: String) -> Int { get } // expected-note{{requirement 'subscript(_:label:)' declared here}}
}

class MislabeledSubscript : P3 {
  subscript(val: String, label: String) -> Int { // expected-error{{method 'subscript' has different argument labels from those required by protocol 'P3' ('subscript(_:label:)')}}
    return 1
  }
}
