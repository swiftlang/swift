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

/// Subscript Super Helpers

class SB {
    subscript() -> Int {
        return 0
    }
    subscript(a: Int) -> Int {
        return a
    }
    subscript(a: Int, b: Int) -> Int {
        return a + b
    }
    subscript(a: Int, b: Int, c: Int) -> Int {
        return a + b + c
    }
    subscript(a: Int, b: Int, c: Int, d: Int) -> Int {
        return a + b + c + d
    }
}

/// Subscripts, Supers, Basic

class SDB0 : SB {
    override subscript() -> Int {
        return super[]
    }
}

class SDB1 : SB {
   override subscript() -> Int {
       return super[int]
    }
}

class SDB2 : SB {
   override subscript() -> Int {
       return super[.typeFoo]
    }
}

class SDB3 : SB {
   override subscript() -> Int {
       return super[0.instanceFoo]
    }
}

class SDB4 : SB {
   override subscript() -> Int {
       return super[(0 as Int).typeFoo] // expected-error{{static member 'typeFoo' cannot be used on instance of type 'Int'}}
    }
}

class SDB5 : SB {
   override subscript() -> Int {
       return super[0, 1]
    }
}

class SDB6 : SB {
   override subscript() -> Int {
       return super[.typeFoo, 1]
    }
}

class SDB7 : SB {
   override subscript() -> Int {
       return super[0, .typeBar]
    }
}

class SDB8 : SB {
   override subscript() -> Int {
       return super[.typeFoo, .typeBar]
    }
}

class SDB9 : SB {
   override subscript() -> Int {
       return super[foo.instanceBar, bar.instanceBaz]
    }
}

/// Subscript, Super, Int Literals, Vertical, Trailing

class SDT1 : SB {
    override subscript(a: Int, b: Int, c: Int, d: Int) -> Int {
        return super[
            a,
            b,
            c,
            d
        ]
    }
}

class SDT2 : SB {
    override subscript(a: Int, b: Int, c: Int, d: Int) -> Int {
        return super[
            a
            b
            c
            d
        ]
    }
}

class SDT3 : SB {
    override subscript(a: Int, b: Int, c: Int, d: Int) -> Int {
        return super[
            a,
            b,
            c
            d
        ]
    }
}

class SDT4 : SB {
    override subscript(a: Int, b: Int, c: Int, d: Int) -> Int {
        return super[
            a,
            b
            c,
            d
        ]
    }
}

class SDT5 : SB {
    override subscript(a: Int, b: Int, c: Int, d: Int) -> Int {
        return super[
            a
            b,
            c,
            d
        ]
    }
}

class SDT6 : SB {
    override subscript(a: Int, b: Int, c: Int, d: Int) -> Int {
        return super[
            a,
            b
            c
            d
        ]
    }
}

class SDT7 : SB {
    override subscript(a: Int, b: Int, c: Int, d: Int) -> Int {
        return super[
            a
            b,
            c
            d
        ]
    }
}

class SDT8 : SB {
    override subscript(a: Int, b: Int, c: Int, d: Int) -> Int {
        return super[
            a
            b
            c,
            d
        ]
    }
}

/// Subscript, Super, Int Literals, Vertical, Leading

class SDL1 : SB {
    override subscript(a: Int, b: Int, c: Int, d: Int) -> Int {
        return super[
            a
          , b
          , c
          , d
        ]
    }
}

class SDL2 : SB {
    override subscript(a: Int, b: Int, c: Int, d: Int) -> Int {
        return super[
            a
            b
            c
            d
        ]
    }
}

class SDL3 : SB {
    override subscript(a: Int, b: Int, c: Int, d: Int) -> Int {
        return super[
            a
          , b
          , c
            d
        ]
    }
}

class SDL4 : SB {
    override subscript(a: Int, b: Int, c: Int, d: Int) -> Int {
        return super[
            a
          , b
            c
          , d
        ]
    }
}

class SDL5 : SB {
    override subscript(a: Int, b: Int, c: Int, d: Int) -> Int {
        return super[
            a
            b
          , c
          , d
        ]
    }
}

class SDL6 : SB {
    override subscript(a: Int, b: Int, c: Int, d: Int) -> Int {
        return super[
            a
          , b
            c
            d
        ]
    }
}

class SDL7 : SB {
    override subscript(a: Int, b: Int, c: Int, d: Int) -> Int {
        return super[
            a
            b
          , c
            d
        ]
    }
}

class SDL8 : SB {
    override subscript(a: Int, b: Int, c: Int, d: Int) -> Int {
        return super[
            a
            b
            c
          , d
        ]
    }
}

/// Subscripts, Super, Variables + Members, Columnar

class SVMC1 : SB {
    func fizz() -> Int {
        return super[
            foo
            .instanceBar
        ]
    }
}

class SVMC2 : SB {
    func fizz() -> Int {
        return super[
            foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
            .typeBar
        ]
    }
}

class SVMC3 : SB {
    func fizz() -> Int {
        return super[
            foo
            bar
            baz
        ]
    }
}

class SVMC4 : SB {
    func fizz() -> Int {
        return super[
            foo
            .instanceBar
            baz
        ]
    }
}

class SVMC5 : SB {
    func fizz() -> Int {
        return super[
            foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
            .typeBar
            baz
        ]
    }
}

class SVMC6 : SB {
    func fizz() -> Int {
        return super[
            foo,
            bar,
            baz
        ]
    }
}

class SVMC7 : SB {
    func fizz() -> Int {
        return super[
            foo
            .instanceBar
            baz
        ]
    }
}

class SVMC8 : SB {
    func fizz() -> Int {
        return super[
            foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
            .typeBar
            baz
        ]
    }
}

class SVMC9 : SB {
    func fizz() -> Int {
        return super[
            .typeFoo
            .typeBar
            .typeBaz // expected-error{{type of expression is ambiguous without more context}}
        ]
    }
}

class SVMC10 : SB {
    func fizz() -> Int {
        return super[
            .typeFoo,
            .typeBar,
            .typeBaz
        ]
    }
}

class SVMC11 : SB {
    func fizz() -> Int {
        return super[
            Int.typeFoo
            Int.typeBar
            Int.typeBaz
        ]
    }
}

/// Tuples, Basics

let tb0: Void = ()

let tb1 = (int)

let tb2: (Int) = (.typeFoo)

let tb3 = (0.instanceFoo)

let tb4 = ((0 as Int).typeFoo) // expected-error{{static member 'typeFoo' cannot be used on instance of type 'Int'}}

let tb5 = (0, 1)

let tb6: (Int, Int) = (.typeFoo, 1)

let tb7: (Int, Int) = (0, .typeBar)

let tb8: (Int, Int) = (.typeFoo, .typeBar)

let tb9 = (foo.instanceBar, bar.instanceBaz)

/// Tuple, Int Literals, Vertical, Trailing

let tilvt1 = (
    0,
    1,
    2,
    3   
)

let tilvt2 = (
    0
    1
    2
    3
)

let tilvt3 = (
    0,
    1,
    2
    3
)

let tilvt4 = (
    0,
    1
    2,
    3
)

let tilvt5 = (
    0
    1,
    2,
    3
)

let tilvt6 = (
    0,
    1
    2
    3
)

let tilvt7 = (
    0
    1,
    2
    3
)

let tilvt8 = (
    0
    1
    2,
    3
)

/// Tuples, Int Literals, Vertical, Trailing

let tilvl1 = (
    0
  , 1
  , 2
  , 3   
)

let tilvl2 = (
    0
    1
    2
    3
)

let tilvl3 = (
    0
  , 1
  , 2
    3
)

let tilvl4 = (
    0
  , 1
    2
  , 3
)

let tilvl5 = (
    0
    1
  , 2
  , 3
)

let tilvl6 = (
    0
  , 1
    2
    3
)

let tilvl7 = (
    0
    1
  , 2
    3
)

let tilvl8 = (
    0
    1
    2
  , 3
)

/// Tuples, Variables + Members, Columnar

let tvmc1 = (
    foo
    .instanceBar
)

let tvmc2 = (
    foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
    .typeBar
)

let tvmc3 = (
    foo
    bar
    baz
)

let tvmc4 = (
    foo
    .instanceBar
    baz
)

let tvmc5 = (
    foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
    .typeBar
    baz
)

let tvmc6 = (
    foo,
    bar,
    baz
)

let tvmc7 = (
    foo
    .instanceBar
    baz
)

let tvmc8 = (
    foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
    .typeBar
    baz
)

let tvmc9 = (
    .typeFoo
    .typeBar
    .typeBaz // expected-error{{type of expression is ambiguous without more context}}
)

let tvmc10: (Int, Int, Int) = (
    .typeFoo,
    .typeBar,
    .typeBaz
)

let tvmc11 = (
    Int.typeFoo
    Int.typeBar
    Int.typeBaz
)

/// Function Helpers

func funcy() -> Int { return 0 }
func funcy(_ a: Int) -> Int { return 0 }
func funcy(_ a: Int, _ b: Int) -> Int { return 0 }
func funcy(_ a: Int, _ b: Int, _ c: Int) -> Int { return 0 }
func funcy(_ a: Int, _ b: Int, _ c: Int, _ d: Int) -> Int { return 0 }

/// Function, Basics

let fb0: Int = funcy()

let fb1: Int = funcy(0)

let fb2: Int = funcy(.typeFoo)

let fb3: Int = funcy(0.instanceFoo)

let fb4: Int = funcy((0 as Int).typeFoo) // expected-error{{static member 'typeFoo' cannot be used on instance of type 'Int'}}

let fb5: Int = funcy(0, 1)

let fb6: Int = funcy(.typeFoo, 1)

let fb7: Int = funcy(0, .typeBar)

let fb8: Int = funcy(.typeFoo, .typeBar)

let fb9: Int = funcy(foo.instanceBar, bar.instanceBaz)

/// Function, Int Literals, Vertical, Trailing

let filvt1: Int = funcy(
    0,
    1,
    2,
    3   
)

let filvt2: Int = funcy(
    0
    1
    2
    3
)

let filvt3: Int = funcy(
    0,
    1,
    2
    3
)

let filvt4: Int = funcy(
    0,
    1
    2,
    3
)

let filvt5: Int = funcy(
    0
    1,
    2,
    3
)

let filvt6: Int = funcy(
    0,
    1
    2
    3
)

let filvt7: Int = funcy(
    0
    1,
    2
    3
)

let filvt8: Int = funcy(
    0
    1
    2,
    3
)

/// Functions, Int Literals, Vertical, Leading

let filvl1: Int = funcy(
    0
  , 1
  , 2
  , 3    
)

let filvl2: Int = funcy(
    0
    1
    2
    3
)

let filvl3: Int = funcy(
    0
  , 1
  , 2
    3
)

let filvl4: Int = funcy(
    0
  , 1
    2
  , 3
)

let filvl5: Int = funcy(
    0
    1
  , 2
  , 3
)

let filvl6: Int = funcy(
    0
  , 1
    2
    3
)

let filvl7: Int = funcy(
    0
    1
  , 2
    3
)

let filvl8: Int = funcy(
    0
    1
    2
  , 3
)

/// Functions, Variables + Members, Columnar

let fvmc1 = funcy(
    foo
    .instanceBar
)

let fvmc2 = funcy(
    foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
    .typeBar
)

let fvmc3 = funcy(
    foo
    bar
    baz
)

let fvmc4 = funcy(
    foo
    .instanceBar
    baz
)

let fvmc5 = funcy(
    foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
    .typeBar
    baz
)

let fvmc6 = funcy(
    foo,
    bar,
    baz
)

let fvmc7 = funcy(
    foo
    .instanceBar
    baz
)

let fvmc8 = funcy(
    foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
    .typeBar
    baz
)

let fvmc9: Int = funcy(
    .typeFoo
    .typeBar
    .typeBaz // expected-error{{type of expression is ambiguous without more context}}
)

let fvmc10: Int = funcy(
    .typeFoo,
    .typeBar,
    .typeBaz
)

let fvmc11: Int = funcy(
    Int.typeFoo
    Int.typeBar
    Int.typeBaz
)

/// Function Super Helpers

class FB {
    func funcy() -> Int {
        return 0
    }
    func funcy(_ a: Int) -> Int {
        return a
    }
    func funcy(_ a: Int, _ b: Int) -> Int {
        return a + b
    }
    func funcy(_ a: Int, _ b: Int, _ c: Int) -> Int {
        return a + b + c
    }
    func funcy(_ a: Int, _ b: Int, _ c: Int, _ d: Int) -> Int {
        return a + b + c + d
    }
}

/// Functions, Supers, Basic

class FDB0 : FB {
    func ffuncy() -> Int {
        return super.funcy()
    }
}

class FDB1 : FB {
   func ffuncy() -> Int {
       return super.funcy(int)
    }
}

class FDB2 : FB {
   func ffuncy() -> Int {
       return super.funcy(.typeFoo)
    }
}

class FDB3 : FB {
   func ffuncy() -> Int {
       return super.funcy(0.instanceFoo)
    }
}

class FDB4 : FB {
   func ffuncy() -> Int {
       return super.funcy((0 as Int).typeFoo) // expected-error{{static member 'typeFoo' cannot be used on instance of type 'Int'}}
    }
}

class FDB5 : FB {
   func ffuncy() -> Int {
       return super.funcy(0, 1)
    }
}

class FDB6 : FB {
   func ffuncy() -> Int {
       return super.funcy(.typeFoo, 1)
    }
}

class FDB7 : FB {
   func ffuncy() -> Int {
       return super.funcy(0, .typeBar)
    }
}

class FDB8 : FB {
   func ffuncy() -> Int {
       return super.funcy(.typeFoo, .typeBar)
    }
}

class FDB9 : FB {
   func ffuncy() -> Int {
       return super.funcy(foo.instanceBar, bar.instanceBaz)
    }
}

/// Function, Super, Int Literals, Vertical, Trailing

class FDT1 : FB {
    override func funcy(_ a: Int, _ b: Int, _ c: Int, _ d: Int) -> Int {
        return super.funcy(
            a,
            b,
            c,
            d
        )
    }
}

class FDT2 : FB {
    override func funcy(_ a: Int, _ b: Int, _ c: Int, _ d: Int) -> Int {
        return super.funcy(
            a
            b
            c
            d
        )
    }
}

class FDT3 : FB {
    override func funcy(_ a: Int, _ b: Int, _ c: Int, _ d: Int) -> Int {
        return super.funcy(
            a,
            b,
            c
            d
        )
    }
}

class FDT4 : FB {
    override func funcy(_ a: Int, _ b: Int, _ c: Int, _ d: Int) -> Int {
        return super.funcy(
            a,
            b
            c,
            d
        )
    }
}

class FDT5 : FB {
    override func funcy(_ a: Int, _ b: Int, _ c: Int, _ d: Int) -> Int {
        return super.funcy(
            a
            b,
            c,
            d
        )
    }
}

class FDT6 : FB {
    override func funcy(_ a: Int, _ b: Int, _ c: Int, _ d: Int) -> Int {
        return super.funcy(
            a,
            b
            c
            d
        )
    }
}

class FDT7 : FB {
    override func funcy(_ a: Int, _ b: Int, _ c: Int, _ d: Int) -> Int {
        return super.funcy(
            a
            b,
            c
            d
        )
    }
}

/// Function, Super, Int Literals, Vertical, Leading

class FDL1 : FB {
    override func funcy(_ a: Int, _ b: Int, _ c: Int, _ d: Int) -> Int {
        return super.funcy(
            a
          , b
          , c
          , d
        )
    }
}

class FDL2 : FB {
    override func funcy(_ a: Int, _ b: Int, _ c: Int, _ d: Int) -> Int {
        return super.funcy(
            a
            b
            c
            d
        )
    }
}

class FDL3 : FB {
    override func funcy(_ a: Int, _ b: Int, _ c: Int, _ d: Int) -> Int {
        return super.funcy(
            a
          , b
          , c
            d
        )
    }
}

class FDL4 : FB {
    override func funcy(_ a: Int, _ b: Int, _ c: Int, _ d: Int) -> Int {
        return super.funcy(
            a
          , b
            c
          , d
        )
    }
}

class FDL5 : FB {
    override func funcy(_ a: Int, _ b: Int, _ c: Int, _ d: Int) -> Int {
        return super.funcy(
            a
            b
          , c
          , d
        )
    }
}

class FDL6 : FB {
    override func funcy(_ a: Int, _ b: Int, _ c: Int, _ d: Int) -> Int {
        return super.funcy(
            a
          , b
            c
            d
        )
    }
}

class FDL7 : FB {
    override func funcy(_ a: Int, _ b: Int, _ c: Int, _ d: Int) -> Int {
        return super.funcy(
            a
            b
          , c
            d
        )
    }
}

class FDL8 : FB {
    override func funcy(_ a: Int, _ b: Int, _ c: Int, _ d: Int) -> Int {
        return super.funcy(
            a
            b
            c
          , d
        )
    }
}

/// Functions, Super, Variables + Members, Columnar

class FVMC1 : FB {
    func fizz() -> Int {
        return super.funcy(
            foo
            .instanceBar
        )
    }
}

class FVMC2 : FB {
    func fizz() -> Int {
        return super.funcy(
            foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
            .typeBar
        )
    }
}

class FVMC3 : FB {
    func fizz() -> Int {
        return super.funcy(
            foo
            bar
            baz
        )
    }
}

class FVMC4 : FB {
    func fizz() -> Int {
        return super.funcy(
            foo
            .instanceBar
            baz
        )
    }
}

class FVMC5 : FB {
    func fizz() -> Int {
        return super.funcy(
            foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
            .typeBar
            baz
        )
    }
}

class FVMC6 : FB {
    func fizz() -> Int {
        return super.funcy(
            foo,
            bar,
            baz
        )
    }
}

class FVMC7 : FB {
    func fizz() -> Int {
        return super.funcy(
            foo
            .instanceBar
            baz
        )
    }
}

class FVMC8 : FB {
    func fizz() -> Int {
        return super.funcy(
            foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
            .typeBar
            baz
        )
    }
}

class FVMC9 : FB {
    func fizz() -> Int {
        return super.funcy(
            .typeFoo
            .typeBar
            .typeBaz // expected-error{{type of expression is ambiguous without more context}}
        )
    }
}

class FVMC10 : FB {
    func fizz() -> Int {
        return super.funcy(
            .typeFoo,
            .typeBar,
            .typeBaz
        )
    }
}

class FVMC11 : FB {
    func fizz() -> Int {
        return super.funcy(
            Int.typeFoo
            Int.typeBar
            Int.typeBaz
        )
    }
}

/// Initializer Helpers

class IB {
    init() {
    }
    init(_ a: Int) {
    }
    init(_ a: Int, _ b: Int) {
    }
    init(_ a: Int, _ b: Int, _ c: Int) {
    }
    init(_ a: Int, _ b: Int, _ c: Int, _ d: Int) {
    }
}

/// Initializer Basics

let ib0 = IB()

let ib1 = IB(int)

let ib2 = IB(.typeFoo)

let ib3 = IB(0.instanceFoo)

let ib4 = IB((0 as Int).typeFoo) // expected-error{{static member 'typeFoo' cannot be used on instance of type 'Int'}}

let ib5 = IB(0, 1)

let ib6 = IB(.typeFoo, 1)

let ib7 = IB(0, .typeBar)

let ib8 = IB(.typeFoo, .typeBar)

let ib9 = IB(foo.instanceBar, bar.instanceBaz)

/// Initializers, Int Literals, Vertical, Trailing

let iilvt1 = IB(
    0,
    1,
    2,
    3   
)

let iilvt2 = IB(
    0
    1
    2
    3
)

let iilvt3 = IB(
    0,
    1,
    2
    3
)

let iilvt4 = IB(
    0,
    1
    2,
    3
)

let iilvt5 = IB(
    0
    1,
    2,
    3
)

let iilvt6 = IB(
    0,
    1
    2
    3
)

let iilvt7 = IB(
    0
    1,
    2
    3
)

let iilvt8 = IB(
    0
    1
    2,
    3
)

/// Initializers, Int Literals, Vertical, Leading

let iilvl1 = IB(
    0
  , 1
  , 2
  , 3    
)

let iilvl2 = IB(
    0
    1
    2
    3
)

let iilvl3 = IB(
    0
  , 1
  , 2
    3
)

let iilvl4 = IB(
    0
  , 1
    2
  , 3
)

let iilvl5 = IB(
    0
    1
  , 2
  , 3
)

let iilvl6 = IB(
    0
  , 1
    2
    3
)

let iilvl7 = IB(
    0
    1
  , 2
    3
)

let iilvl8 = IB(
    0
    1
    2
  , 3
)

/// Initializers, Variables + Members, Columnar

let ivmc1 = IB(
    foo
    .instanceBar
)

let ivmc2 = IB(
    foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
    .typeBar
)

let ivmc3 = IB(
    foo
    bar
    baz
)

let ivmc4 = IB(
    foo
    .instanceBar
    baz
)

let ivmc5 = IB(
    foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
    .typeBar
    baz
)

let ivmc6 = IB(
    foo,
    bar,
    baz
)

let ivmc7 = IB(
    foo
    .instanceBar
    baz
)

let ivmc8 = IB(
    foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
    .typeBar
    baz
)

let ivmc9 = IB(
    .typeFoo
    .typeBar
    .typeBaz // expected-error{{type of expression is ambiguous without more context}}
)

let ivmc10 = IB(
    .typeFoo,
    .typeBar,
    .typeBaz
)

let ivmc11 = IB(
    Int.typeFoo
    Int.typeBar
    Int.typeBaz
)

/// Initializers, Supers, Basic

class IDB0 : IB {
    override init() {
        super.init()
    }
}

class IDB1 : IB {
   override init() {
       super.init(int)
    }
}

class IDB2 : IB {
   override init() {
       super.init(.typeFoo)
    }
}

class IDB3 : IB {
   override init() {
       super.init(0.instanceFoo)
    }
}

class IDB4 : IB {
   override init() {
       super.init((0 as Int).typeFoo) // expected-error{{static member 'typeFoo' cannot be used on instance of type 'Int'}}
    }
}

class IDB5 : IB {
   override init() {
       super.init(0, 1)
    }
}

class IDB6 : IB {
   override init() {
       super.init(.typeFoo, 1)
    }
}

class IDB7 : IB {
   override init() {
       super.init(0, .typeBar)
    }
}

class IDB8 : IB {
   override init() {
       super.init(.typeFoo, .typeBar)
    }
}

class IDB9 : IB {
   override init() {
       super.init(foo.instanceBar, bar.instanceBaz)
    }
}

/// Initializers, Supers, Int Literals, Vertical, Trailing

class IDT1 : IB {
    override init(_ a: Int, _ b: Int, _ c: Int, _ d: Int) {
        super.init(
            a,
            b,
            c,
            d
        )
    }
}

class IDT2 : IB {
    override init(_ a: Int, _ b: Int, _ c: Int, _ d: Int) {
        super.init(
            a
            b
            c
            d
        )
    }
}

class IDT3 : IB {
    override init(_ a: Int, _ b: Int, _ c: Int, _ d: Int) {
        super.init(
            a,
            b,
            c
            d
        )
    }
}

class IDT4 : IB {
    override init(_ a: Int, _ b: Int, _ c: Int, _ d: Int) {
        super.init(
            a,
            b
            c,
            d
        )
    }
}

class IDT5 : IB {
    override init(_ a: Int, _ b: Int, _ c: Int, _ d: Int) {
        super.init(
            a
            b,
            c,
            d
        )
    }
}

class IDT6 : IB {
    override init(_ a: Int, _ b: Int, _ c: Int, _ d: Int) {
        super.init(
            a,
            b
            c
            d
        )
    }
}

class IDT7 : IB {
    override init(_ a: Int, _ b: Int, _ c: Int, _ d: Int) {
        super.init(
            a
            b,
            c
            d
        )
    }
}

/// Initializers, Super, Int Literals, Leading

class IDL1 : IB {
    override init(_ a: Int, _ b: Int, _ c: Int, _ d: Int) {
        super.init(
            a
          , b
          , c
          , d
        )
    }
}

class IDL2 : IB {
    override init(_ a: Int, _ b: Int, _ c: Int, _ d: Int) {
        super.init(
            a
            b
            c
            d
        )
    }
}

class IDL3 : IB {
    override init(_ a: Int, _ b: Int, _ c: Int, _ d: Int) {
        super.init(
            a
          , b
          , c
            d
        )
    }
}

class IDL4 : IB {
    override init(_ a: Int, _ b: Int, _ c: Int, _ d: Int) {
        super.init(
            a
          , b
            c
          , d
        )
    }
}

class IDL5 : IB {
    override init(_ a: Int, _ b: Int, _ c: Int, _ d: Int) {
        super.init(
            a
            b
          , c
          , d
        )
    }
}

class IDL6 : IB {
    override init(_ a: Int, _ b: Int, _ c: Int, _ d: Int) {
        super.init(
            a
          , b
            c
            d
        )
    }
}

class IDL7 : IB {
    override init(_ a: Int, _ b: Int, _ c: Int, _ d: Int) {
        super.init(
            a
            b
          , c
            d
        )
    }
}

class IDL8 : IB {
    override init(_ a: Int, _ b: Int, _ c: Int, _ d: Int) {
        super.init(
            a
            b
            c
          , d
        )
    }
}

/// Initializers, Super, Variables + Members, Columnar

class IVMC1 : IB {
    override init() {
        super.init(
            foo
            .instanceBar
        )
    }
}

class IVMC2 : IB {
    override init() {
        super.init(
            foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
            .typeBar
        )
    }
}

class IVMC3 : IB {
    override init() {
        super.init(
            foo
            bar
            baz
        )
    }
}

class IVMC4 : IB {
    override init() {
        super.init(
            foo
            .instanceBar
            baz
        )
    }
}

class IVMC5 : IB {
    override init() {
        super.init(
            foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
            .typeBar
            baz
        )
    }
}

class IVMC6 : IB {
    override init() {
        super.init(
            foo,
            bar,
            baz
        )
    }
}

class IVMC7 : IB {
    override init() {
        super.init(
            foo
            .instanceBar
            baz
        )
    }
}

class IVMC8 : IB {
    override init() {
        super.init(
            foo // expected-error{{static member 'typeBar' cannot be used on instance of type 'Int'}}
            .typeBar
            baz
        )
    }
}

class IVMC9 : IB {
    override init() {
        super.init(
            .typeFoo
            .typeBar
            .typeBaz // expected-error{{type of expression is ambiguous without more context}}
        )
    }
}

class IVMC10 : IB {
    override init() {
        super.init(
            .typeFoo,
            .typeBar,
            .typeBaz
        )
    }
}

class IVMC11 : IB {
    override init() {
        super.init(
            Int.typeFoo
            Int.typeBar
            Int.typeBaz
        )
    }
}



/// Contextual Keywords

/// Helpers

func ff_contextualArgument_associatedtype(a: Int, b: Int, c: Int, associatedtype: Int, d: Int) {}
func ff_contextualArgument_class(a: Int, b: Int, c: Int, class: Int, d: Int) {}
func ff_contextualArgument_deinit(a: Int, b: Int, c: Int, deinit: Int, d: Int) {}
func ff_contextualArgument_enum(a: Int, b: Int, c: Int, enum: Int, d: Int) {}
func ff_contextualArgument_extension(a: Int, b: Int, c: Int, extension: Int, d: Int) {}
func ff_contextualArgument_func(a: Int, b: Int, c: Int, func: Int, d: Int) {}
func ff_contextualArgument_import(a: Int, b: Int, c: Int, import: Int, d: Int) {}
func ff_contextualArgument_init(a: Int, b: Int, c: Int, init: Int, d: Int) {}
func ff_contextualArgument_inout(a: Int, b: Int, c: Int, `inout`: Int, d: Int) {}
func ff_contextualArgument_let(a: Int, b: Int, c: Int, `let`: Int, d: Int) {}
func ff_contextualArgument_operator(a: Int, b: Int, c: Int, `operator`: Int, d: Int) {}
func ff_contextualArgument_precedencegroup(a: Int, b: Int, c: Int, precedencegroup: Int, d: Int) {}
func ff_contextualArgument_protocol(a: Int, b: Int, c: Int, protocol: Int, d: Int) {}
func ff_contextualArgument_struct(a: Int, b: Int, c: Int, struct: Int, d: Int) {}
func ff_contextualArgument_subscript(a: Int, b: Int, c: Int, subscript: Int, d: Int) {}
func ff_contextualArgument_typealias(a: Int, b: Int, c: Int, typealias: Int, d: Int) {}
func ff_contextualArgument_var(a: Int, b: Int, c: Int, `var`: Int, d: Int) {}
func ff_contextualArgument_fileprivate(a: Int, b: Int, c: Int, fileprivate: Int, d: Int) {}
func ff_contextualArgument_internal(a: Int, b: Int, c: Int, internal: Int, d: Int) {}
func ff_contextualArgument_private(a: Int, b: Int, c: Int, private: Int, d: Int) {}
func ff_contextualArgument_public(a: Int, b: Int, c: Int, public: Int, d: Int) {}
func ff_contextualArgument_static(a: Int, b: Int, c: Int, static: Int, d: Int) {}
func ff_contextualArgument_defer(a: Int, b: Int, c: Int, defer: Int, d: Int) {}
func ff_contextualArgument_if(a: Int, b: Int, c: Int, if: Int, d: Int) {}
func ff_contextualArgument_guard(a: Int, b: Int, c: Int, guard: Int, d: Int) {}
func ff_contextualArgument_do(a: Int, b: Int, c: Int, do: Int, d: Int) {}
func ff_contextualArgument_repeat(a: Int, b: Int, c: Int, repeat: Int, d: Int) {}
func ff_contextualArgument_else(a: Int, b: Int, c: Int, else: Int, d: Int) {}
func ff_contextualArgument_for(a: Int, b: Int, c: Int, for: Int, d: Int) {}
func ff_contextualArgument_in(a: Int, b: Int, c: Int, in: Int, d: Int) {}
func ff_contextualArgument_while(a: Int, b: Int, c: Int, while: Int, d: Int) {}
func ff_contextualArgument_return(a: Int, b: Int, c: Int, return: Int, d: Int) {}
func ff_contextualArgument_break(a: Int, b: Int, c: Int, break: Int, d: Int) {}
func ff_contextualArgument_continue(a: Int, b: Int, c: Int, continue: Int, d: Int) {}
func ff_contextualArgument_fallthrough(a: Int, b: Int, c: Int, fallthrough: Int, d: Int) {}
func ff_contextualArgument_switch(a: Int, b: Int, c: Int, switch: Int, d: Int) {}
func ff_contextualArgument_case(a: Int, b: Int, c: Int, case: Int, d: Int) {}
func ff_contextualArgument_default(a: Int, b: Int, c: Int, default: Int, d: Int) {}
func ff_contextualArgument_where(a: Int, b: Int, c: Int, where: Int, d: Int) {}
func ff_contextualArgument_catch(a: Int, b: Int, c: Int, catch: Int, d: Int) {}
func ff_contextualArgument_throw(a: Int, b: Int, c: Int, throw: Int, d: Int) {}
func ff_contextualArgument_as(a: Int, b: Int, c: Int, as: Int, d: Int) {}
func ff_contextualArgument_Any(a: Int, b: Int, c: Int, Any: Int, d: Int) {}
func ff_contextualArgument_false(a: Int, b: Int, c: Int, false: Int, d: Int) {}
func ff_contextualArgument_is(a: Int, b: Int, c: Int, is: Int, d: Int) {}
func ff_contextualArgument_nil(a: Int, b: Int, c: Int, nil: Int, d: Int) {}
func ff_contextualArgument_rethrows(a: Int, b: Int, c: Int, rethrows: Int, d: Int) {}
func ff_contextualArgument_super(a: Int, b: Int, c: Int, super: Int, d: Int) {}
func ff_contextualArgument_self(a: Int, b: Int, c: Int, self: Int, d: Int) {}
func ff_contextualArgument_Self(a: Int, b: Int, c: Int, Self: Int, d: Int) {}
func ff_contextualArgument_true(a: Int, b: Int, c: Int, true: Int, d: Int) {}
func ff_contextualArgument_try(a: Int, b: Int, c: Int, try: Int, d: Int) {}
func ff_contextualArgument_throws(a: Int, b: Int, c: Int, throws: Int, d: Int) {}
func ff_contextualArgument___FILE__(a: Int, b: Int, c: Int, __FILE__: Int, d: Int) {}
func ff_contextualArgument___LINE__(a: Int, b: Int, c: Int, __LINE__: Int, d: Int) {}
func ff_contextualArgument___COLUMN__(a: Int, b: Int, c: Int, __COLUMN__: Int, d: Int) {}
func ff_contextualArgument___FUNCTION__(a: Int, b: Int, c: Int, __FUNCTION__: Int, d: Int) {}
func ff_contextualArgument___DSO_HANDLE__(a: Int, b: Int, c: Int, __DSO_HANDLE__: Int, d: Int) {}
func ff_contextualArgument___(a: Int, b: Int, c: Int, __: Int, d: Int) {}
func ff_contextualArgument_keyPath(a: Int, b: Int, c: Int, keyPath: Int, d: Int) {}
func ff_contextualArgument_line(a: Int, b: Int, c: Int, line: Int, d: Int) {}
func ff_contextualArgument_selector(a: Int, b: Int, c: Int, selector: Int, d: Int) {}
func ff_contextualArgument_file(a: Int, b: Int, c: Int, file: Int, d: Int) {}
func ff_contextualArgument_column(a: Int, b: Int, c: Int, column: Int, d: Int) {}
func ff_contextualArgument_function(a: Int, b: Int, c: Int, function: Int, d: Int) {}
func ff_contextualArgument_dsohandle(a: Int, b: Int, c: Int, dsohandle: Int, d: Int) {}
func ff_contextualArgument_sourceLocation(a: Int, b: Int, c: Int, sourceLocation: Int, d: Int) {}
func ff_contextualArgument_warning(a: Int, b: Int, c: Int, warning: Int, d: Int) {}
func ff_contextualArgument_error(a: Int, b: Int, c: Int, error: Int, d: Int) {}
func ff_contextualArgument_elseif(a: Int, b: Int, c: Int, elseif: Int, d: Int) {}
func ff_contextualArgument_endif(a: Int, b: Int, c: Int, endif: Int, d: Int) {}
func ff_contextualArgument_available(a: Int, b: Int, c: Int, available: Int, d: Int) {}
func ff_contextualArgument_fileLiteral(a: Int, b: Int, c: Int, fileLiteral: Int, d: Int) {}
func ff_contextualArgument_imageLiteral(a: Int, b: Int, c: Int, imageLiteral: Int, d: Int) {}
func ff_contextualArgument_colorLiteral(a: Int, b: Int, c: Int, colorLiteral: Int, d: Int) {}
func ff_contextualArgument_integer_literal(a: Int, b: Int, c: Int, integer_literal: Int, d: Int) {}
func ff_contextualArgument_floating_literal(a: Int, b: Int, c: Int, floating_literal: Int, d: Int) {}
func ff_contextualArgument_string_literal(a: Int, b: Int, c: Int, string_literal: Int, d: Int) {}
func ff_contextualArgument_unknown(a: Int, b: Int, c: Int, unknown: Int, d: Int) {}
func ff_contextualArgument_identifier(a: Int, b: Int, c: Int, identifier: Int, d: Int) {}
func ff_contextualArgument_oper_binary_unspaced(a: Int, b: Int, c: Int, oper_binary_unspaced: Int, d: Int) {}
func ff_contextualArgument_oper_binary_spaced(a: Int, b: Int, c: Int, oper_binary_spaced: Int, d: Int) {}
func ff_contextualArgument_oper_prefix(a: Int, b: Int, c: Int, oper_prefix: Int, d: Int) {}
func ff_contextualArgument_dollarident(a: Int, b: Int, c: Int, dollarident: Int, d: Int) {}
func ff_contextualArgument_contextual_keyword(a: Int, b: Int, c: Int, contextual_keyword: Int, d: Int) {}
func ff_contextualArgument_string_segment(a: Int, b: Int, c: Int, string_segment: Int, d: Int) {}
func ff_contextualArgument_string_interpolation_anchor(a: Int, b: Int, c: Int, string_interpolation_anchor: Int, d: Int) {}
func ff_contextualArgument_yield(a: Int, b: Int, c: Int, yield: Int, d: Int) {}

ff_contextualArgument_associatedtype(
    a: 1
    b: 1
    c: 1
    associatedtype: 1
    d: 1
)

ff_contextualArgument_class(
    a: 1
    b: 1
    c: 1
    class: 1
    d: 1
)

ff_contextualArgument_deinit(
    a: 1
    b: 1
    c: 1
    deinit: 1
    d: 1
)

ff_contextualArgument_enum(
    a: 1
    b: 1
    c: 1
    enum: 1
    d: 1
)

ff_contextualArgument_extension(
    a: 1
    b: 1
    c: 1
    extension: 1
    d: 1
)

ff_contextualArgument_func(
    a: 1
    b: 1
    c: 1
    func: 1
    d: 1
)

ff_contextualArgument_import(
    a: 1
    b: 1
    c: 1
    import: 1
    d: 1
)

ff_contextualArgument_init(
    a: 1
    b: 1
    c: 1
    init: 1
    d: 1
)

ff_contextualArgument_inout(
    a: 1
    b: 1
    c: 1
    `inout`: 1
    d: 1
)

ff_contextualArgument_let(
    a: 1
    b: 1
    c: 1
    `let`: 1
    d: 1
)

ff_contextualArgument_operator(
    a: 1
    b: 1
    c: 1
    operator: 1
    d: 1
)

ff_contextualArgument_precedencegroup(
    a: 1
    b: 1
    c: 1
    precedencegroup: 1
    d: 1
)

ff_contextualArgument_protocol(
    a: 1
    b: 1
    c: 1
    protocol: 1
    d: 1
)

ff_contextualArgument_struct(
    a: 1
    b: 1
    c: 1
    struct: 1
    d: 1
)

ff_contextualArgument_subscript(
    a: 1
    b: 1
    c: 1
    subscript: 1
    d: 1
)

ff_contextualArgument_typealias(
    a: 1
    b: 1
    c: 1
    typealias: 1
    d: 1
)

ff_contextualArgument_var(
    a: 1
    b: 1
    c: 1
    `var`: 1
    d: 1
)

ff_contextualArgument_fileprivate(
    a: 1
    b: 1
    c: 1
    fileprivate: 1
    d: 1
)

ff_contextualArgument_internal(
    a: 1
    b: 1
    c: 1
    internal: 1
    d: 1
)

ff_contextualArgument_private(
    a: 1
    b: 1
    c: 1
    private: 1
    d: 1
)

ff_contextualArgument_public(
    a: 1
    b: 1
    c: 1
    public: 1
    d: 1
)

ff_contextualArgument_static(
    a: 1
    b: 1
    c: 1
    static: 1
    d: 1
)

ff_contextualArgument_defer(
    a: 1
    b: 1
    c: 1
    defer: 1
    d: 1
)

ff_contextualArgument_if(
    a: 1
    b: 1
    c: 1
    if: 1
    d: 1
)

ff_contextualArgument_guard(
    a: 1
    b: 1
    c: 1
    guard: 1
    d: 1
)

ff_contextualArgument_do(
    a: 1
    b: 1
    c: 1
    do: 1
    d: 1
)

ff_contextualArgument_repeat(
    a: 1
    b: 1
    c: 1
    repeat: 1
    d: 1
)

ff_contextualArgument_else(
    a: 1
    b: 1
    c: 1
    else: 1
    d: 1
)

ff_contextualArgument_for(
    a: 1
    b: 1
    c: 1
    for: 1
    d: 1
)

ff_contextualArgument_in(
    a: 1
    b: 1
    c: 1
    in: 1
    d: 1
)

ff_contextualArgument_while(
    a: 1
    b: 1
    c: 1
    while: 1
    d: 1
)

ff_contextualArgument_return(
    a: 1
    b: 1
    c: 1
    return: 1
    d: 1
)

ff_contextualArgument_break(
    a: 1
    b: 1
    c: 1
    break: 1
    d: 1
)

ff_contextualArgument_continue(
    a: 1
    b: 1
    c: 1
    continue: 1
    d: 1
)

ff_contextualArgument_fallthrough(
    a: 1
    b: 1
    c: 1
    fallthrough: 1
    d: 1
)

ff_contextualArgument_switch(
    a: 1
    b: 1
    c: 1
    switch: 1
    d: 1
)

ff_contextualArgument_case(
    a: 1
    b: 1
    c: 1
    case: 1
    d: 1
)

ff_contextualArgument_default(
    a: 1
    b: 1
    c: 1
    default: 1
    d: 1
)

ff_contextualArgument_where(
    a: 1
    b: 1
    c: 1
    where: 1
    d: 1
)

ff_contextualArgument_catch(
    a: 1
    b: 1
    c: 1
    catch: 1
    d: 1
)

ff_contextualArgument_throw(
    a: 1
    b: 1
    c: 1
    throw: 1
    d: 1
)

ff_contextualArgument_as(
    a: 1
    b: 1
    c: 1
    as: 1
    d: 1
)

ff_contextualArgument_Any(
    a: 1
    b: 1
    c: 1
    Any: 1
    d: 1
)

ff_contextualArgument_false(
    a: 1
    b: 1
    c: 1
    false: 1
    d: 1
)

ff_contextualArgument_is(
    a: 1
    b: 1
    c: 1
    is: 1
    d: 1
)

ff_contextualArgument_nil(
    a: 1
    b: 1
    c: 1
    nil: 1
    d: 1
)

ff_contextualArgument_rethrows(
    a: 1
    b: 1
    c: 1
    rethrows: 1
    d: 1
)

ff_contextualArgument_super(
    a: 1
    b: 1
    c: 1
    super: 1
    d: 1
)

ff_contextualArgument_self(
    a: 1
    b: 1
    c: 1
    self: 1
    d: 1
)

ff_contextualArgument_Self(
    a: 1
    b: 1
    c: 1
    Self: 1
    d: 1
)

ff_contextualArgument_true(
    a: 1
    b: 1
    c: 1
    true: 1
    d: 1
)

ff_contextualArgument_try(
    a: 1
    b: 1
    c: 1
    try: 1
    d: 1
)

ff_contextualArgument_throws(
    a: 1
    b: 1
    c: 1
    throws: 1
    d: 1
)

ff_contextualArgument___FILE__(
    a: 1
    b: 1
    c: 1
    __FILE__: 1
    d: 1
)

ff_contextualArgument___LINE__(
    a: 1
    b: 1
    c: 1
    __LINE__: 1
    d: 1
)

ff_contextualArgument___COLUMN__(
    a: 1
    b: 1
    c: 1
    __COLUMN__: 1
    d: 1
)

ff_contextualArgument___FUNCTION__(
    a: 1
    b: 1
    c: 1
    __FUNCTION__: 1
    d: 1
)

ff_contextualArgument___DSO_HANDLE__(
    a: 1
    b: 1
    c: 1
    __DSO_HANDLE__: 1
    d: 1
)

ff_contextualArgument___(
    a: 1
    b: 1
    c: 1
    __: 1
    d: 1
)

ff_contextualArgument_keyPath(
    a: 1
    b: 1
    c: 1
    keyPath: 1
    d: 1
)

ff_contextualArgument_line(
    a: 1
    b: 1
    c: 1
    line: 1
    d: 1
)

ff_contextualArgument_selector(
    a: 1
    b: 1
    c: 1
    selector: 1
    d: 1
)

ff_contextualArgument_file(
    a: 1
    b: 1
    c: 1
    file: 1
    d: 1
)

ff_contextualArgument_column(
    a: 1
    b: 1
    c: 1
    column: 1
    d: 1
)

ff_contextualArgument_function(
    a: 1
    b: 1
    c: 1
    function: 1
    d: 1
)

ff_contextualArgument_dsohandle(
    a: 1
    b: 1
    c: 1
    dsohandle: 1
    d: 1
)

ff_contextualArgument_sourceLocation(
    a: 1
    b: 1
    c: 1
    sourceLocation: 1
    d: 1
)

ff_contextualArgument_warning(
    a: 1
    b: 1
    c: 1
    warning: 1
    d: 1
)

ff_contextualArgument_error(
    a: 1
    b: 1
    c: 1
    error: 1
    d: 1
)

ff_contextualArgument_elseif(
    a: 1
    b: 1
    c: 1
    elseif: 1
    d: 1
)

ff_contextualArgument_endif(
    a: 1
    b: 1
    c: 1
    endif: 1
    d: 1
)

ff_contextualArgument_available(
    a: 1
    b: 1
    c: 1
    available: 1
    d: 1
)

ff_contextualArgument_fileLiteral(
    a: 1
    b: 1
    c: 1
    fileLiteral: 1
    d: 1
)

ff_contextualArgument_imageLiteral(
    a: 1
    b: 1
    c: 1
    imageLiteral: 1
    d: 1
)

ff_contextualArgument_colorLiteral(
    a: 1
    b: 1
    c: 1
    colorLiteral: 1
    d: 1
)

ff_contextualArgument_integer_literal(
    a: 1
    b: 1
    c: 1
    integer_literal: 1
    d: 1
)

ff_contextualArgument_floating_literal(
    a: 1
    b: 1
    c: 1
    floating_literal: 1
    d: 1
)

ff_contextualArgument_string_literal(
    a: 1
    b: 1
    c: 1
    string_literal: 1
    d: 1
)

ff_contextualArgument_unknown(
    a: 1
    b: 1
    c: 1
    unknown: 1
    d: 1
)

ff_contextualArgument_identifier(
    a: 1
    b: 1
    c: 1
    identifier: 1
    d: 1
)

ff_contextualArgument_oper_binary_unspaced(
    a: 1
    b: 1
    c: 1
    oper_binary_unspaced: 1
    d: 1
)

ff_contextualArgument_oper_binary_spaced(
    a: 1
    b: 1
    c: 1
    oper_binary_spaced: 1
    d: 1
)

ff_contextualArgument_oper_prefix(
    a: 1
    b: 1
    c: 1
    oper_prefix: 1
    d: 1
)

ff_contextualArgument_dollarident(
    a: 1
    b: 1
    c: 1
    dollarident: 1
    d: 1
)

ff_contextualArgument_contextual_keyword(
    a: 1
    b: 1
    c: 1
    contextual_keyword: 1
    d: 1
)

ff_contextualArgument_string_segment(
    a: 1
    b: 1
    c: 1
    string_segment: 1
    d: 1
)

ff_contextualArgument_string_interpolation_anchor(
    a: 1
    b: 1
    c: 1
    string_interpolation_anchor: 1
    d: 1
)

ff_contextualArgument_yield(
    a: 1
    b: 1
    c: 1
    yield: 1
    d: 1
)


