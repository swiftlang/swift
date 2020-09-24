// RUN: %swift-indent %s >%t.response
// RUN: diff -u %s %t.response


// Trailing '.'s should be indented as if they were a dot member expression.
//
HStack(alignment: .center) {
    landmark.image
        .
}
.
; // Make sure the trailing . doesn't use the next token as its member
//identifier.


// Parameters should align by their external argument names
//
func foobar(
    record: Int?,
    forKeys keys: [String] = Foo.bar,
    atIndex index: Int,
    error: NSError?)
{
}


// Closures within arguments should align their closing brace to match the
// argument indentation
//
Base.query(
    predicate: foo,
    shouldShowMessage: { () -> Bool in
        fatalError()
    },
    retryTarget: foo,
    retrySelector: foo,
    networkUnavailable: {
        fatalError()
    },
    unknownError: {
        fatalError()
    }
)

// Arguments that span multiple lines should indent relative to the argument start.
// FIXME: Should unlabelled args following a labelled arg be indented relative
//        to it?
//
let children = KeyValuePairs<String, Any>(dictionaryLiteral:
                                            ("something", "Oh Hello!"),
                                          ("something", "Oh Hello!"),
                                          ("something", "Oh Hello!")
)


// Trailing closure content should indent relative to the called function,
// rather than starting brace.
//
test(arg1: 1,
     arg2: 2) { x in
    print(x)
}

test(arg1: 1,
     arg2: 2)
{ x in
    print(x)
}

let x = [1, 2, 3]
    .filter {$0 < $1}
    .filter {$0 < $1}
    .filter {$0 < $1}

bax(34949494949)
    .foo(a: Int,
         b: Int){
    }[x: {
        fatalError()
    }] {
        fatalError()
    }
    .baz


// Enum element parameters should be aligned, and raw values should be indented.

enum TestEnum {
    case first(x: Int,
               y: Int,
               z: Int),
         second(
            x: Int,
            y: Int
         )
    case third
}

enum RawEnum: String {
    case aCaseWithAParticularlyLongNameSoTheValueIsWrapped =
            "a long message here",
         aNotherCaseWithAParticularlyLongNameSoTheValueIsWrapped =
            "a long message here"
}


// Condition elements should align with each other.
//
guard let x = Optional.some(10), x > 100,
      let y = Optional.some(20), y < 50,
      #available(OSX 10.9, iOS 7.0, *),
      x < y
else {
    fatalError()
}

if let x = Optional.some(10), x > 100,
   let y = Optional.some(20), y < 50,
   #available(OSX 10.9, iOS 7.0, *),
   x < y
{
    fatalError()
}

guard #available(
    OSX 10.9, iOS 7.0, *
), x < y else {
    fatalError()
}

if #available(
    OSX 10.9, iOS 7.0, *
), x < y {
    fatalError()
}

// Trailing closures, subscript expressions and argument tuples/parens should
// indent relative to the line containing the start of the last named component
// of their function, or the function start if their are none. Any child
// tuples/parens/brackets should that start after them on the same line should
// do the same.
//
let _ = []
    .map {
        f {
            print()
        } ?? 0
    }

basename
    .foo(a: Int,
         b: Int) [x: {
        fatalError()
    }] {
        fatalError()
    }

basename
    .foo(a: Int,
         b: Int) ({
        fatalError()
    }) {
        fatalError()
    }

basename
    .foo()
    {
        fatalError()
    }

[foo(a: Int,
     b: Int)[z: {
    fatalError()
}],
"hello"
]

[foo(a: Int,
     b: Int)[z: foo ({
    fatalError()
}, {
    fatalError()
})],
"hello"
]

[foo(a: Int,
     b: Int) {
    fatalError()
} [y: 10,
   z: foo ({
    fatalError()
   }, {
    fatalError()
   })],
"hello"
]

[foo(a: Int,
     b: Int) [z: foo ()
{
    fatalError()
})],
"hello"
]

[foo(a: Int,
     b: Int) [z: foo (a: 1,
                      b: 2)
{
    fatalError()
})],
"hello"
]


// Closing arg parens shouldn't indent.
//
func foo(
    bar: BarParameterType,
    baz: BazParameterType
) -> SomeResultType {
    fatalError()
}


// Sequence expressions should indent realtive to their first element. Leading
// "=" should be considered part of the sequence when present.
//
let arrayA = [0]
let arrayB = [1]
let arrayC = [2]
let arrayD = [3]

let array1 =
    arrayA +
    arrayB +
    arrayC +
    arrayD

array1 =
    arrayA +
    arrayB +
    arrayC +
    arrayD

array1 = (
    2, 3, 4
).0 +
arrayA +
arrayB +
arrayC +
arrayD

let array2 = arrayA +
    arrayB +
    arrayC +
    arrayD


// Comments should not break exact alignment, and leading comments should be aligned, rather than the label.
//
_ = NSLocalizedString("Call without a comment", // here is a comment
                      comment: "This indents correctly") // here is a comment
_ = NSLocalizedString("Call with a comment",
                      // Here is a comment
                      comment: "Note the bad indentation")
_ = NSLocalizedString(first: "Call with a comment",
                      /*leading*/second: "foobarbaz",
                      next: "Here is the next element")


// String interpolations shouldn't break exact alignment.
//
struct Foo {
    let value1: String
    let value2: String
    let value3: String

    static func makeFooList() -> [Foo] {
        let foo1 = Foo(value1: "Blue",
                       value2: "House",
                       value3: "Chicago")
        let foo2 = Foo(value1: "Blue \(Date())",
                       value2: "House",
                       value3: "Chicago")
        let foo3 = Foo(value1: "Blue",
                       value2: "House \(Date())",
                       value3: "Chicago")
        let foo4 = Foo(value1: "Blue",
                       value2: "House",
                       value3: "Chicago \(Date())")
        let foo5 = Foo(value1: "Blue " + Date().description,
                       value2: "House",
                       value3: "Chicago \(Date())")
        return [foo1, foo2, foo3, foo4, foo5]
    }
}



// Multiline string leading whitespace should be preserved.
//
let s = """
    a
        b
            c
    """

func wantsToIndentContents() {
    let dontLetItIndentMyValue = """
a
    b
        c
"""
}

print("""
    foo {
        bar()
    }
    """)


// Interpolations shouldn't change how multiline strings are handled.
//
switch self {
case .first:
    return """
        foo foo foo foo foo foo foo foo foo foo foo foo foo foo foo foo foo foo foo \
        foo foo foo foo foo foo foo foo foo foo foo foo foo foo foo foo foo foo foo.
        """
case .second(let a, let b):
    return """
        foo foo foo foo foo foo foo foo foo foo foo foo foo foo foo foo \(bar.bar), \
        foo foo foo foo foo foo foo foo foo foo foo foo foo foo foo foo \(bar.bar).
        """
}


// Comments after the last item of a collection should still indent.
//
let x = [
    // hello
    124,
    // hello
    123
    // hello
]


// Pound directives aren't indentation contexts, though this should probably be
// configurable.
//
#if UNIT_TEST
var array: [String] = []
#else
var array: [String] = {
    return ["one",
            "two"]
}()
#endif
#if os(iOS)
var source: String? {
    if true {
        if otherCondition {
            return "true"
        }
    }
}
#endif


// Comments should not affect switch case indentations.
//
switch false {
case true:
    break // a comment
case false:
    break // another comment
}

// The else in guard statements should not be indented.
//
guard
    let a = boo,
    let b = foo,
    let c = woo
else
{
    fatalError()
}

// Trailing commas in collections expressions shouldn't cause the closing
// bracket to indent.
//
func a() {
    let b: [String] = [
        "x",
    ]
}
func a() {
    let b: [String: Int] = [
        "x": 1,
    ]
}


// Array/Dictionay/Tuple/Closure within arg list shouldn't cause their solo
// closing tokens to indent.
//
foo(a: [
    "hi": "b"
])
foo(a: [
    456
])
AssertNoThrow({
    let x = 1
}())
bar((
    x: 1,
    y: 2
))


// Else on a new line shouldn't indent.
//
if contains(.Stuff) {
    foo()
} else
if contains(.Things) {
    bar()
} else
if contains(.Objects) {
    baz()
} else
if contains(.Concepts) {
    bill()
}


// Base classes, or conformed-to protocols should be indented at the same level.
//
class Bar:
    Foo,
    Fizz,
    Buzz {
    func foo() {}
}

struct Bar: Boop,
            Beep,
            Bop {
    func foo() {}
}


// Chaining after literals should not break their closing token's indentation.
//
let array2 = [
    42,
    69,
].map { $0 + 1 }

let array3 = [
    42,
    69,
].map {
    $0 + 1
}

let array4 = [
    42,
    69,
].map {
    $0.distance(
        to: -1
    )
}


// Capture lists should not affect how the surrounding code is indented.
//
foo(first: { [weak self] _ in
    print("")
}, second: { _ in
    print("")
})


// Comments between case statements aren't within them.
//
switch foo {
case .bar: return

// a comment
case .baz: return

// a comment
// on two lines
case .buz: return
}


// Arrays within exactly-aligned lists should indent relative to the alignment column.
//
private static let domainFooo = FoooodOntologyNode(name: "food",
                                                   childNodes: [
                                                    first,
                                                    second,
                                                    third,
                                                    fourth,
                                                    Base.sixth
                                                    Base.seventh.two
                                                   ],
                                                   isSpecial: false) {}

// Incomplete member access shouldn't cause indentation to fail.
//
["one",
 "two"]
    .foo {

    } (
        x: 10,
        y: 94
    ).
; // Make sure later code isn't interpreted as the member name


// Elements that span from the opening tuple line should prevent later elements indenting.
//
baz(foo(a: 34,
        b: 56) {
    print("hello")
},
bar)


// Generic parameter list elements should be aligned.
//
struct Foo<T: Equatable &
            Hashable,
           U: Identifiable,
           V: Codable & Decodable> {

}


// Where clauses requirements should be aligned.
//
struct Foo<T, U, V> where T: Equatable & Hashable,
                          U: Identifiable,
                          V: Codable &
                            Decodable {

}

// Generic parameter list elements should be aligned.
//
struct Foo<T: Equatable & Hashable,
           U: Identifiable,
           V: Codable & Decodable> where T: Equatable & Hashable,
                                         U: Identifiable,
                                         V: Codable & Decodable  {

}


// The closing ] of array literals shouldn't indent regardless of whether a trailing member access is present or not.
//
let array2 = [
    42,
    69,
].map { $0 + 1 }


// Make sure we handle the where clause on 'for' correctly.
//
for value in array where
    value > 0
{
    print(value)
}

for value in array where
    value > 0 {
    print(value)
}

for value in array
where value > 0 {
    print(value)
}

for
    value
in
    array
where
    value > 0 &&
    value < 2 {
    print(value)
}


for value in array
where
    value > 0 &&
    value < 2 {
    print(value)
}

for value in array where
    value > 0 &&
    value < 2 {
    print(value)
}


// Only "top-level" expressions should indent when split across multiple lines.
//
func foo() {
    (1 + 2 + 4)
        + otherTerm

    func inner(a b: Int = (1 + 2 + 4)
                + otherTerm) {}

    func inner(a b: Int =
                (1 + 2 + 4)
                + otherTerm) {}

    if
        (1 + 2 + 4)
            + otherTerm > 10,
        (3 + 5 + 6)
            + otherTerm < 30 {
        print("foo")
    }

    let x = (
        (1 + 2 + 4)
            + otherTerm > 10
    )

    let y = (
        (1 + 2 + 4)
            + otherTerm > 10,
        label:
            (1 + 2 + 4)
            + otherTerm > 10
    )

    foo {
        (1 + 2 + 4)
            + otherTerm
    }

    return
        (1 + 2 + 4)
        + otherTerm
}


// Dictionary element values should be indented relative to their key.
//
_ = [
    "foo": "bar",
    "foo":
        "bar"
]
_ = [
    (1, 2): (1, 2),
    (1, 2):
        (1, 2),
    (1, 2): (
        1,
        2
    ),
    (
        1,
        2
    ):
        (1, 2),
    (
        1,
        2
    ): (
        1,
        2
    ),
    (1, 2):
        (
            1,
            2
        )
]


// Closure params and capture list should be indented from the closure body, and their elements aligned.
//

_ = {
    [x, y] (a, b) -> Int in
    fatalError()
}

_ = {
    (a: Int,
     b: Int) -> Int in
    fatalError()
}

_ = {
    (a: Int,
     b: Int
    ) -> Int in
    fatalError()
}

_ = {
    [x, y] a,
           b -> Int in
    fatalError()
}

_ = {
    [x, y] (a: (
        Int, Int
    ),
    b) -> Int in
    fatalError()
}

_ = {[x, y] (a: (
    Int, Int
),
b) -> Int in
    fatalError()
}

_ = {
    [x,
     y,
     ] a,
       b -> Int in
    fatalError()
}
_ = {
    [weak myX = y.x,
     unowned yourX = y.p]() in
    fatalError()
}

_ = {[
    weak myX = y.x,
    unowned yourX = y.p
]() in
    fatalError()
}

_ = {[
    weak myX =
        y.x,
    unowned yourX =
        y.p
]() in
    fatalError()
}

_ = {[
    weak myX =
        y.x,
    unowned yourX =
        y.p
]() in ((
    45
))}


// Tuple types should have their elements aligned exactly.
//
let x: (
    Int,
    Int
)? = nil

let x: (
    a: Int,
    b: Int
)? = nil

let x: (
    a:
        Int,
    b:
        Int
)? = nil

let x: (Int,
        Int)? = nil

let x: (a: Int,
        b: Int)? = nil

let x: (a:
            Int,
        b:
            Int)? = nil

let x: (Int,
        Int
)? = nil

let x: (a: Int,
        b: Int
)? = nil

let x: (a:
            Int,
        b:
            Int
)? = nil


let myFunc: (Int,
             Int,
             Int
) -> Int

let myFunc: () -> (Int,
                   Int,
                   Int)


// Tuple types should align on their outer label locs (if present)
//
typealias Thing = (_ tmpURL: URL,
                   _ secondURL: URL) -> (destinationURL: URL, options: Options)

// Type member access shouldn't indent.
//
func foo(
    a: Type
        .Type,
    b: Type
        .Protocol
) {}

func foo(
    a:
        Type
        .Type,
    b:
        Type
        .Protocol,
    c:
        [[
            Type
            .Type
        ]:
            String
        ]
) {}

var (d, e):
    (Int, Int) = (1, 3),
    (f, g): (
        Int,
        Int
    ) = (1, 3),
    (h, i):
        (
            Int,
            Int
        ) = (1, 3)

var (d,
     e
):
    (Int, Int) = (1, 3),
(f,
 g
): (
    Int,
    Int
) = (1, 3)

// Generic identifier types should have their arguments aligned.
//
let x: Array<Int,
             String,
             Int>

let x: Array<
    Int,
    String,
    Int
>

let x: Array<(
    Int,
    String,
    Int
)>

// Generic specializations should have their arguments aligned too
//
let x = foo<Int,
            String,
            Int>()

let x = foo<
    Int,
    String,
    Int
>()
.filter { $0 > 10 }
.count


// Invalid elements should still be indented.
//
let x: [Int?] = [
    .
]


// Attributes on case stmts shouldn't indent their case
//
switch Foo.c {
case .a:
    break
case .b:
    break;
case .c:
    break
@unknown
default:
    break
}


// Handle empty single expression closures (invalid start location)
//
foo.bar() {
    return
}


// Handle invalid for each (missing 'in' location)
//
for query: foo
bar;


// Handle custom attributes
//
@Custom(foo: 3,
        baz: 3)
struct Foo {
    @Custom(
        foo: Bar,
        baz: Bar
    )
    var d: Int = 10
}

// Ignore postfix expressions when determining context locations.
//
return Foo(deferred) {
    print("hello")
}++

IncrementedFirst++
    .foo()++
    .bar {

    }++
    .baz()


// Multiple patterns in catch should align exactly.

do {
    print("hello")
} catch MyErr.a(let code, let message),
        MyErr.b(
            let code,
            let message
        ),
        MyErr.c(let code, let message) {
    print("ahhh!")
}

do {
    throw MyErr.a
} catch where foo == 0,
        where bar == 1 {
}

do
{
    print("hello")
}
catch MyErr.a(let code, let message),
      MyErr.b(
        let code,
        let message
      ),
      MyErr.c(let code, let message)
{
    print("ahhh!")
}

// Pattern binding decls should only column-align if no element spans from the first line to beyond it.

public let x = 10,
           y = 20

private var firstThing = 20,
            secondThing = item
                .filter {},
            thirdThing = 42

public let myVar = itemWithALongName
    .filter { $0 >= $1 && $0 - $1 < 50}

public let first = 45, second = itemWithALongName
    .filter { $0 >= $1 && $0 - $1 < 50}

private var secondThing = item
    .filter {},
    firstThing = 20,
    thirdThing = 56


// Function decls missing their parameter list and body shouldn't pick up the next token as a continuation.

struct BarType {
    func bar
}

struct <#name#> {
    <#fields#>
}

struct <#name#> {
    <#fields#>
    func foo() {}
}


// Array literal elements should have their continuation lines indented relative to their first line.

doStuffWithList([
    baseThing()
        .map { $0 }
        .append(\.sdfsdf),
    secondItem
        .filter {$0 < 10}
])
