// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple

// REQUIRES: objc_interop

enum E : Error {
  case DoesNotExist
}

class Vertex {
  var marked : Bool = false

  // FIXME: we should suppress this note about adding @objc, since that's not allowed!

  // expected-note@+2 {{add '@objc' to expose this property to Objective-C}}
  // expected-note@+1 5 {{property declared here}}
  var parent : Vertex {
    get throws { throw E.DoesNotExist }
  }

  // expected-note@+1 2 {{subscript declared here}}
  subscript(_ i : Int) -> Vertex? {
    get async throws {
      if i == 0 {
        return try? parent
      }
      return Vertex()
    }
  }
}

func asFunction(_ f : (Vertex) -> Bool) {}

let _ = \Vertex.parent.parent.parent // expected-error 3 {{cannot form key path to property with 'throws' or 'async'}}
let _ = \Vertex.[3]?.marked // expected-error {{cannot form key path to subscript with 'throws' or 'async'}}

asFunction(\Vertex.[0]!.marked) // expected-error {{cannot form key path to subscript with 'throws' or 'async'}}
asFunction(\Vertex.parent.marked) // expected-error {{cannot form key path to property with 'throws' or 'async'}}

// expected-error@+2 {{argument of '#keyPath' refers to non-'@objc' property 'parent'}}
// expected-error@+1 {{cannot form key path to property with 'throws' or 'async'}}
let _ = #keyPath(Vertex.parent)

