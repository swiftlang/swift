// RUN: %target-typecheck-verify-swift

/// Helpers

let foo: Int = 0
let bar: Int = 1
let baz: Int = 2

extension Int {
    var instanceFoo: Int { return 0 }
    var instanceBar: Int { return 1 }
    var instanceBaz: Int { return 2 }
    static var typeFoo: Int { return 0 }
    static var typeBar: Int { return 1 }
    static var typeBaz: Int { return 2 }
}

/// Array Literals, Basics

let ab0: [Int] = []

let ab1: [Int] = [0]

let ab2: [Int] = [.typeFoo]

let ab3: [Int] = [0.instanceFoo]

let ab4: [Int] = [(0 as Int).typeFoo] // expected-error{{static member 'typeFoo' cannot be used on instance of type 'Int'}}

let ab5: [Int] = [0, 1]

let ab6: [Int] = [.typeFoo, 1]

let ab7: [Int] = [0, .typeBar]

let ab8: [Int] = [.typeFoo, .typeBar]

let ab9: [Int] = [foo.instanceBar, bar.instanceBaz]

/// Array Literals, Int Literals, Vertical, Trailing

let a1: [Int] = [
    0,
    1,
    2,
    3    
]

let a2: [Int] = [
    0
    1
    2
    3
]

let a3: [Int] = [
    0,
    1,
    2
    3
]

let a4: [Int] = [
    0,
    1
    2,
    3
]

let a5: [Int] = [
    0
    1,
    2,
    3
]

let a6: [Int] = [
    0,
    1
    2
    3
]

let a7: [Int] = [
    0
    1,
    2
    3
]

let a8: [Int] = [
    0
    1
    2,
    3
]

/// Array Literals, Int Literals, Vertical, Leading

let al1: [Int] = [
    0
  , 1
  , 2
  , 3    
]

let al2: [Int] = [
    0
    1
    2
    3
]

let al3: [Int] = [
    0
  , 1
  , 2
    3
]

let al4: [Int] = [
    0
  , 1
    2
  , 3
]

let al5: [Int] = [
    0
    1
  , 2
  , 3
]

let al6: [Int] = [
    0
  , 1
    2
    3
]

let al7: [Int] = [
    0
    1
  , 2
    3
]

let al8: [Int] = [
    0
    1
    2
  , 3
]

/// Array Literals, Int Literals, Wide Vertical, Trailing

let aw1: [Int] = [

    0,

    1,

    2,

    3    

]

let aw2: [Int] = [

    0

    1

    2

    3

]

let aw3: [Int] = [

    0,

    1,

    2

    3

]

let aw4: [Int] = [

    0,

    1

    2,

    3

]

let aw5: [Int] = [

    0

    1,

    2,

    3

]

let aw6: [Int] = [

    0,

    1

    2

    3

]

let aw7: [Int] = [

    0

    1,

    2

    3

]

let aw8: [Int] = [

    0

    1

    2,

    3

]

// Array Literals, Int Literals, Mixed

let am1: [Int] = [
    0, 1,
    2
    3
]

let am2: [Int] = [
    0, 1
    2
    3
]

let am3: [Int] = [
    0, 1, 2,
    3
]

let am4: [Int] = [
    0, 1, 2
    3
]

/// Array Literals, Variables + Members, Columnar

let av1 = [
    foo
    .instanceBar
]

let av2 = [
    foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
    .typeBar
]

let av3 = [
    foo
    bar
    baz
]

let av4 = [
    foo
    .instanceBar
    baz
]

let av5 = [
    foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
    .typeBar
    baz
]

let av6 = [
    foo,
    bar,
    baz
]

let av7 = [
    foo
    .instanceBar
    baz
]

let av8 = [
    foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
    .typeBar
    baz
]

let av9: [Int] = [
    .typeFoo
    .typeBar
    .typeBaz // expected-error{{type of expression is ambiguous without more context}}
]

let av10: [Int] = [
    .typeFoo,
    .typeBar,
    .typeBaz
]

let av11: [Int] = [
    Int.typeFoo
    Int.typeBar
    Int.typeBaz
]


/// Dictionary Literals

let d1: [Int : Int] = [
    0 : 0,
    1 : 1,
    2 : 2,
    3 : 3
]

let d2: [Int : Int] = [
    0 : 0
    1 : 1
    2 : 2
    3 : 3
]

let d3: [Int : Int] = [
    0 : 0,
    1 : 1,
    2 : 2
    3 : 3
]

let d4: [Int : Int] = [
    0 : 0,
    1 : 1
    2 : 2,
    3 : 3
]

let d5: [Int : Int] = [
    0 : 0
    1 : 1,
    2 : 2,
    3 : 3
]

let d6: [Int : Int] = [
    0 : 0,
    1 : 1
    2 : 2
    3 : 3
]

let d7: [Int : Int] = [
    0 : 0
    1 : 1,
    2 : 2
    3 : 3
]

let d8: [Int : Int] = [
    0 : 0
    1 : 1
    2 : 2,
    3 : 3
]

/// Dictionary Literals, Int Literals, Vertical, Leading

let dl1: [Int : Int] = [
    0 : 0
  , 1 : 1
  , 2 : 2
  , 3 : 3    
]

let dl2: [Int : Int] = [
    0 : 0
    1 : 1
    2 : 2
    3 : 3
]

let dl3: [Int : Int] = [
    0 : 0
  , 1 : 1
  , 2 : 2
    3 : 3
]

let dl4: [Int : Int] = [
    0 : 0
  , 1 : 1
    2 : 2
  , 3 : 3
]

let dl5: [Int : Int] = [
    0 : 0
    1 : 1
  , 2 : 2
  , 3 : 3
]

let dl6: [Int : Int] = [
    0 : 0
  , 1 : 1
    2 : 2
    3 : 3
]

let dl7: [Int : Int] = [
    0 : 0
    1 : 1
  , 2 : 2
    3 : 3
]

let dl8: [Int : Int] = [
    0 : 0
    1 : 1
    2 : 2
  , 3 : 3
]

/// Dictionary Literals, Variables + Members, Columnar

let dv1 = [
    foo : foo
    .instanceBar
]

let dv2 = [
    foo : foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
    .typeBar
]

let dv3 = [
    foo : foo
    bar : bar
    baz : baz
]

let dv4 = [
    foo : foo
    .instanceBar
    baz : baz
]

let dv5 = [
    foo : foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
    .typeBar
    baz : baz
]

let dv6 = [
    foo : foo,
    bar : bar,
    baz : baz
]

let dv7 = [
    foo : foo
    .instanceBar
    baz : baz
]

let dv8 = [
    foo : foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
    .typeBar
    baz : baz
]

let dv9: [Int : Int] = [
    foo : .typeFoo
    .typeBar
    .typeBaz // expected-error{{type of expression is ambiguous without more context}}
]

let dv10: [Int : Int] = [
    foo : .typeFoo,
    bar : .typeBar,
    baz : .typeBaz
]

let dv11: [Int : Int] = [
    foo : Int.typeFoo
    bar : Int.typeBar
    baz : Int.typeBaz
]


/// Subscript Helpers

extension Int {
    subscript() -> Int { return 0 }
    subscript(first: Int) -> Int { return 0 }
    subscript(first: Int, second: Int) -> Int { return 0 }
    subscript(first: Int, second: Int, third: Int) -> Int { return 0 }
    subscript(first: Int, second: Int, third: Int, fourth: Int) -> Int { return 0 }
}

let int: Int = -1

/// Subscript, Basics

let sb0: Int = int[]

let sb1: Int = int[0]

let sb2: Int = int[.typeFoo]

let sb3: Int = int[0.instanceFoo]

let sb4: Int = int[(0 as Int).typeFoo] // expected-error{{static member 'typeFoo' cannot be used on instance of type 'Int'}}

let sb5: Int = int[0, 1]

let sb6: Int = int[.typeFoo, 1]

let sb7: Int = int[0, .typeBar]

let sb8: Int = int[.typeFoo, .typeBar]

let sb9: Int = int[foo.instanceBar, bar.instanceBaz]

/// Subscript, Int Literals, Vertical, Trailing

let s1: Int = int[
    0,
    1,
    2,
    3   
]

let s2: Int = int[
    0
    1
    2
    3
]

let s3: Int = int[
    0,
    1,
    2
    3
]

let s4: Int = int[
    0,
    1
    2,
    3
]

let s5: Int = int[
    0
    1,
    2,
    3
]

let s6: Int = int[
    0,
    1
    2
    3
]

let s7: Int = int[
    0
    1,
    2
    3
]

let s8: Int = int[
    0
    1
    2,
    3
]

/// Subscripts, Int Literals, Vertical, Leading

let sl1: Int = int[
    0
  , 1
  , 2
  , 3    
]

let sl2: Int = int[
    0
    1
    2
    3
]

let sl3: Int = int[
    0
  , 1
  , 2
    3
]

let sl4: Int = int[
    0
  , 1
    2
  , 3
]

let sl5: Int = int[
    0
    1
  , 2
  , 3
]

let sl6: Int = int[
    0
  , 1
    2
    3
]

let sl7: Int = int[
    0
    1
  , 2
    3
]

let sl8: Int = int[
    0
    1
    2
  , 3
]

/// Subscripts, Variables + Members, Columnar

let sv1 = int[
    foo
    .instanceBar
]

let sv2 = int[
    foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
    .typeBar
]

let sv3 = int[
    foo
    bar
    baz
]

let sv4 = int[
    foo
    .instanceBar
    baz
]

let sv5 = int[
    foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
    .typeBar
    baz
]

let sv6 = int[
    foo,
    bar,
    baz
]

let sv7 = int[
    foo
    .instanceBar
    baz
]

let sv8 = int[
    foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
    .typeBar
    baz
]

let sv9: Int = int[
    .typeFoo
    .typeBar
    .typeBaz // expected-error{{type of expression is ambiguous without more context}}
]

let sv10: Int = int[
    .typeFoo,
    .typeBar,
    .typeBaz
]

let sv11: Int = int[
    Int.typeFoo
    Int.typeBar
    Int.typeBaz
]
