// RUN: %target-typecheck-verify-swift

struct SomeRange { }

// Function declarations.
func paramName(_ func: Int, in: SomeRange) { }
func firstArgumentLabelWithParamName(in range: SomeRange) { }
func firstArgumentLabelWithParamName2(range in: SomeRange) { }

func escapedInout(`inout` value: SomeRange) { }

struct SomeType {
  // Initializers
  init(func: () -> ()) { }
  init(init func: () -> ()) { }

  // Subscripts
  subscript (class index: AnyClass) -> Int { 
    return 0
  }

  subscript (class: AnyClass) -> Int { 
    return 0
  }

  subscript (struct: Any.Type) -> Int { 
    return 0
  }
}

class SomeClass { }

// Function types.
typealias functionType = (_ in: SomeRange) -> Bool

// Calls
func testCalls(_ range: SomeRange) {
  paramName(0, in: range)
  firstArgumentLabelWithParamName(in: range)
  firstArgumentLabelWithParamName2(range: range)
  var st = SomeType(func: {})
  st = SomeType(init: {})
  _ = st[class: SomeClass.self]
  _ = st[SomeClass.self]
  _ = st[SomeType.self]

  escapedInout(`inout`: range)

  // Fix-Its
  paramName(0, `in`: range) // expected-warning{{keyword 'in' does not need to be escaped in argument list}}{{16-17=}}{{19-20=}}
}
