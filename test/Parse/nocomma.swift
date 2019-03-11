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

