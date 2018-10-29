// RUN: %target-repl-run-simple-swift | %FileCheck %s

// REQUIRES: swift_repl

:print_decl String
// CHECK: struct String
// CHECK: extension String : _ExpressibleByStringInterpolation

false // CHECK: Bool = false
(1,2) // CHECK: (Int, Int) = (1, 2)
print(10)
print(10)
// CHECK: 10
// CHECK-NEXT: 10

func f() {
  print("Hello")
}
f() // CHECK: Hello

102 // CHECK: Int = 102

var x = 1 // CHECK: x : Int = 1

(x, 2)  // CHECK: (Int, Int) = (1, 2)

var (_, y) = (x,2)  // CHECK: (_, y) : (Int, Int) = (1, 2)

var aString = String() // CHECK: aString : String = ""

"\n\r\t\"' abcd\u{45}\u{01}"  // CHECK: "\n\r\t\"\' abcdE\u{01}"


String(-1) // CHECK: String = "-1"
String(-112312312)  // CHECK: String = "-112312312"
String(4.0)  // CHECK: String = "4.0"

1 +
44
// CHECK: Int = 45{{$}}

123 .
hashValue
// CHECK: Int = {{-?[0-9]+$}}

// Check that we handle unmatched parentheses in REPL.
1+1)

var z = 44
+z
// CHECK: Int = 44{{$}}

+44
// CHECK: Int = 44{{$}}

typealias Foo = Int

var f1 : Foo = 1
var f44 : Foo = 44
f1 +
  f44
// CHECK: Foo = 45{{$}}
+(f44)
// CHECK: Foo = 44{{$}}

1.5
// CHECK: Double = 1.5{{$}}
1.5+2.25
// CHECK: Double = 3.75{{$}}
+1.75
// CHECK: Double = 1.75{{$}}
-1.75
// CHECK: Double = -1.75{{$}}

func r13792487(_ x: Float64) -> Float64 { return x }
r13792487(1234.0)
// CHECK: Float64 = 1234.0{{$}}
r13792487(1234)
// CHECK: Float64 = 1234.0{{$}}

var ab = (1,
2)
// CHECK: (Int, Int) = (1, 2)

var ba = (y:1, x:2)

var f2 : Foo = 2
var xy = (f1, f2)
// CHECK: (Foo, Foo) = (1, 2)

var yx = (y:f1, x:f2)

func sub(x x: Int, y: Int) -> Int { return x - y }

sub(x:1, y:2) // CHECK: Int = -1
sub(x:f1, y:f2) // CHECK: Foo = -1

var array = [1, 2, 3, 4, 5]
// CHECK: array : [Int] = [1, 2, 3, 4, 5]

var dict = [ "Hello" : 1.5 ]
// CHECK: dict : [String : Double] = ["Hello": 1.5]

0..<10
// FIXME: Disabled CHECK for Range<Int> = 0...10 until we get general printing going
// FIXME: Disabled CHECK for 0...10 pending <rdar://problem/11510876>
// (Implement overload resolution)

// Don't crash on this. rdar://14912363
-2...-1

-2 ... -1
// r2 : Range<Int> = -2...-1

String(0)
// CHECK: "0"

for i in 0..<10 { print(i); }
// CHECK: 0
// CHECK-NEXT: 1
// CHECK-NEXT: 2
// CHECK-NEXT: 3
// CHECK-NEXT: 4
// CHECK-NEXT: 5
// CHECK-NEXT: 6
// CHECK-NEXT: 7
// CHECK-NEXT: 8
// CHECK-NEXT: 9

for i in 0..<10 { print(i); }
// CHECK: 0
// CHECK-NEXT: 1
// CHECK-NEXT: 2
// CHECK-NEXT: 3
// CHECK-NEXT: 4
// CHECK-NEXT: 5
// CHECK-NEXT: 6
// CHECK-NEXT: 7
// CHECK-NEXT: 8
// CHECK-NEXT: 9

for c in "foobar".unicodeScalars { print(c) }
// CHECK: f
// CHECK-NEXT: o
// CHECK-NEXT: o
// CHECK-NEXT: b
// CHECK-NEXT: a
// CHECK-NEXT: r

var vec = Array<String>()
// CHECK: vec : [String] = []

// Error recovery
var a : [int]
var a = [Int]()
var b = a.slice[3..<5]

struct Inner<T> {}
struct Outer<T> { var inner : Inner<T> }
Outer<Int>(inner: Inner()) // CHECK: Outer<Int> = REPL_{{.+}}.Outer

struct ContainsSlice { var slice : [Int] }
ContainsSlice(slice: [1, 2, 3]) // CHECK: ContainsSlice = REPL_{{.+}}.ContainsSlice

struct ContainsGenericSlice<T> { var slice : [T] }
ContainsGenericSlice(slice: [1, 2, 3]) // CHECK: ContainsGenericSlice<Int> = REPL_{{.+}}.ContainsGenericSlice
ContainsGenericSlice(slice: [(1, 2), (3, 4)]) // CHECK: ContainsGenericSlice<(Int, Int)> = REPL_{{.+}}.ContainsGenericSlice

struct ContainsContainsSlice { var containsSlice : ContainsSlice }
ContainsContainsSlice(containsSlice: ContainsSlice(slice: [1, 2, 3])) // CHECK: ContainsContainsSlice = REPL_{{.+}}.ContainsContainsSlice

protocol Proto {
  func foo()
}
extension Double : Proto { 
  func foo() {
    print("Double: \(self)\n", terminator: "") 
  }
}
var pr : Proto = 3.14159
// CHECK: Double: 3.14159
pr.foo()

// erroneous; we'll try again after adding the conformance
pr = "foo"

extension String : Proto {
  func foo() {
    print("String: \(self)\n", terminator: "")
  }
}
pr = "foo"
// CHECK: String: foo
pr.foo()

var _ : ([Int]).Type = type(of: [4])
// CHECK: : ([Int]).Type
var _ : ((Int) -> Int)? = .none
// CHECK: : ((Int) -> Int)?
func chained(f f: @escaping (Int) -> ()) -> Int { return 0 }
chained
// CHECK: : (@escaping (Int) -> ()) -> Int
[chained]
// CHECK: : [(@escaping (Int) -> ()) -> Int]

({97210}())
// CHECK: = 97210
true && true
// CHECK: = true

if ({true}()) { print("yeah1") }
// CHECK: yeah1
if true && true { print("yeah2") }
// CHECK: yeah2
if true && true { if true && true { print("yeah3") } }
// CHECK: yeah3
if true && (true && true) { if true && (true && true) { print("yeah4") } }
// CHECK: yeah4
if true && true { if true && true { print(true && true) } }
// CHECK: true

"ok"
// CHECK: = "ok"

