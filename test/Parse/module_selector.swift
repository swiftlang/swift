// RUN: %target-swift-frontend -typecheck -verify -module-name main -enable-experimental-feature ModuleSelectors %s

// RUN: not %target-swift-frontend -parse -verify %s 2>/dev/null

func boolToInt(_ x: Swift::Bool) -> Swift::Int {
  return x ? 1 : 0
}

struct GoodStruct {
  var x: Swift::Int
}

enum GoodEnum {
  case goodCase(Swift::Int)
}

extension GoodStruct: Swift::Equatable {}

extension Swift::Int {
  var magnitude: Swift::Int {
    Swift::abs(self)
  }

  init(goodStruct: main::GoodStruct) {
    self.init(goodStruct.x)
  }
}

let shadowMe: Swift::Int = 2

func usesModuleSelectors() {
  let _: Swift::Double = 1.0
  let _: (Swift::Int, Swift::Bool) = (1, true)
  // FIXME: Support qualification of operators
//  let _: (Swift::Int, Swift::Int) -> Swift::Int = (Swift::+)
  let goodStruct: main::GoodStruct = main::GoodStruct(x: .Swift::min)
  let _: Int = Swift::Int.main::init(goodStruct: goodStruct)

  if Swift::Bool.Swift::random() {
    Swift::fatalError()
  }

  let shadowMe = main::shadowMe
  _ = shadowMe
}

func whitespace() {
  _ = Swift::print
  _ = Swift:: print
  _ = Swift ::print
  _ = Swift :: print
  _ = Swift::
        print
  _ = Swift
        ::print
  _ = Swift ::
        print
  _ = Swift
        :: print
  _ = Swift
        ::
        print
}

let x: Swift::Int = 1
Swift::print(x)

// FIXME: Test property wrappers, result builders

func main::badFunc() {}
// expected-error@-1 {{name in function declaration cannot be qualified with a module selector}}
// expected-note@-2 {{remove module selector from this name}} {{6-12=}}

enum main::BadEnum {
  // expected-error@-1 {{name in enum declaration cannot be qualified with a module selector}}
  // expected-note@-2 {{remove module selector from this name}} {{6-12=}}

  case main::badCase
  // expected-error@-1 {{name in enum 'case' declaration cannot be qualified with a module selector}}
  // expected-note@-2 {{remove module selector from this name}} {{8-14=}}
}

struct main::BadStruct {}
// expected-error@-1 {{name in struct declaration cannot be qualified with a module selector}}
// expected-note@-2 {{remove module selector from this name}} {{8-14=}}

class main::BadClass {}
// expected-error@-1 {{name in class declaration cannot be qualified with a module selector}}
// expected-note@-2 {{remove module selector from this name}} {{7-13=}}

typealias main::BadTypealias = Bool
// expected-error@-1 {{name in typealias declaration cannot be qualified with a module selector}}
// expected-note@-2 {{remove module selector from this name}} {{11-17=}}

protocol main::BadProto {
  // expected-error@-1 {{name in protocol declaration cannot be qualified with a module selector}}
  // expected-note@-2 {{remove module selector from this name}} {{10-16=}}

  associatedtype main::BadAssociatedType
  // expected-error@-1 {{name in associatedtype declaration cannot be qualified with a module selector}}
  // expected-note@-2 {{remove module selector from this name}} {{18-24=}}
}

extension GoodStruct {
  func main::badMethod() {}
  // expected-error@-1 {{name in function declaration cannot be qualified with a module selector}}
  // expected-note@-2 {{remove module selector from this name}} {{8-14=}}

  enum main::BadNestedEnum {
    // expected-error@-1 {{name in enum declaration cannot be qualified with a module selector}}
    // expected-note@-2 {{remove module selector from this name}} {{8-14=}}

    case main::badCaseInNestedEnum
    // expected-error@-1 {{name in enum 'case' declaration cannot be qualified with a module selector}}
    // expected-note@-2 {{remove module selector from this name}} {{10-16=}}
  }

  struct main::BadNestedStruct {}
  // expected-error@-1 {{name in struct declaration cannot be qualified with a module selector}}
  // expected-note@-2 {{remove module selector from this name}} {{10-16=}}

  class main::BadNestedClass {}
  // expected-error@-1 {{name in class declaration cannot be qualified with a module selector}}
  // expected-note@-2 {{remove module selector from this name}} {{9-15=}}

  typealias main::BadNestedTypealias = Bool
  // expected-error@-1 {{name in typealias declaration cannot be qualified with a module selector}}
  // expected-note@-2 {{remove module selector from this name}} {{13-19=}}
}

struct InvalidModuleSelectors {
  var a: ::Int
  // expected-error@-1 {{expected identifier in module selector}}

  var b: (::Int)
  // expected-error@-1 {{expected identifier in module selector}}

  var c: *::Int
  // expected-error@-1 {{expected identifier in module selector}}

  var d: _::Int
  // expected-error@-1 {{expected identifier in module selector}}

  var e: Self::Int
  // expected-error@-1 {{expected identifier in module selector}}

  var f: self::Int
  // expected-error@-1 {{expected identifier in module selector}}

  var g: inout::Int
  // expected-error@-1 {{expected identifier in module selector}}

  var h: Any::Int
  // expected-error@-1 {{expected identifier in module selector}}

  var aArray: [::Int]
  // expected-error@-1 {{expected identifier in module selector}}

  var bArray: [(::Int)]
  // expected-error@-1 {{expected identifier in module selector}}

  var cArray: [*::Int]
  // expected-error@-1 {{expected identifier in module selector}}

  var dArray: [_::Int]
  // expected-error@-1 {{expected identifier in module selector}}

  var eArray: [Self::Int]
  // expected-error@-1 {{expected identifier in module selector}}

  var fArray: [self::Int]
  // expected-error@-1 {{expected identifier in module selector}}

  var gArray: [inout::Int]
  // expected-error@-1 {{expected identifier in module selector}}

  var hArray: [Any::Int]
  // expected-error@-1 {{expected identifier in module selector}}

  var aIndex: String.::Index
  // expected-error@-1 {{expected identifier in module selector}}

  var bIndex: String._::Index
  // expected-error@-1 {{expected identifier in module selector}}

  var cIndex: String.Self::Index
  // expected-error@-1 {{expected identifier in module selector}}

  var dIndex: String.self::Index
  // expected-error@-1 {{expected identifier in module selector}}

  var eIndex: String.inout::Index
  // expected-error@-1 {{expected identifier in module selector}}

  var fIndex: String.Any::Index
  // expected-error@-1 {{expected identifier in module selector}}

  func inExpr(_ s: GoodStruct) {
    _ = s.::x
    // expected-error@-1 {{expected identifier in module selector}}

    _ = 1.::magnitude
    // expected-error@-1 {{expected identifier in module selector}}

    _ = (1.::magnitude)
    // expected-error@-1 {{expected identifier in module selector}}

    Self::print()
    // expected-error@-1 {{expected identifier in module selector}}

    self::print()
    // expected-error@-1 {{expected identifier in module selector}}

    _ = 1._::magnitude
    // expected-error@-1 {{expected identifier in module selector}}

    _ = 1.Self::magnitude
    // expected-error@-1 {{expected identifier in module selector}}

    _ = 1.inout::magnitude
    // expected-error@-1 {{expected identifier in module selector}}

    _ = 1.Any::magnitude
    // expected-error@-1 {{expected identifier in module selector}}
  }
}

/*
func inExpr(_ s: GoodStruct) {
  ::print()
  (::print())
  *::print()
  _::print()
  inout::print()
  Any::print()
  // FIXME: This gets interpreted as a single `.*` operator; may not be ideal.
  _ = 1.*::magnitude
  _ = 1.self::magnitude
}
*/

/*
struct InvalidModuleSelectors {
 // FIXME: This gets interpreted as a single `.*` operator; may not be ideal.
 var gIndex: String.*::Index
}
*/
