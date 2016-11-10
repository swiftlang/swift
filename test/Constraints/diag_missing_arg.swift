// RUN: %target-parse-verify-swift

func nonNamedFunc(_ x: Int) {} // expected-note * {{here}}
nonNamedFunc() // expected-error {{missing argument for parameter #1 in call}} {{14-14=<#T##Int#>}}

func namedFunc(x: Int) {} // expected-note * {{here}}
namedFunc() // expected-error {{missing argument for parameter 'x' in call}} {{11-11=x: <#T##Int#>}}

func inoutFunc(x: inout Int) {} // expected-note * {{here}}
inoutFunc() // expected-error {{missing argument for parameter 'x' in call}} {{11-11=x: &<#T##Int#>}}

func autoclosureFunc(x: @autoclosure () -> Int) {} // expected-note * {{here}}
autoclosureFunc() // expected-error {{missing argument for parameter 'x' in call}} {{17-17=x: <#T##Int#>}}

func attributedFunc(x: @convention(c) () -> Int32) {} // expected-note * {{here}}
attributedFunc() // expected-error {{missing argument for parameter 'x' in call}} {{16-16=x: <#T##@convention(c) () -> Int32#>}}

func attributedInOutFunc(x: inout @convention(c) () -> Int32) {} // expected-note * {{here}}
attributedInOutFunc() // expected-error {{missing argument for parameter 'x' in call}} {{21-21=x: &<#T##@convention(c) () -> Int32#>}}

func genericFunc1<T>(x: T) {} // expected-note * {{here}}
genericFunc1() // expected-error {{missing argument for parameter 'x' in call}} {{14-14=x: <#T##T#>}}

protocol P {}
func genericFunc2<T : P>(x: T) {} // expected-note * {{here}}
genericFunc2() // expected-error {{missing argument for parameter 'x' in call}} {{14-14=x: <#T##T#>}}

typealias MyInt = Int
func aliasedFunc(x: MyInt) {} // expected-note * {{here}}
aliasedFunc() // expected-error {{missing argument for parameter 'x' in call}} {{13-13=x: <#T##MyInt#>}}

func trailingClosureSingle1(x: Int, y: () -> Int) {} // expected-note * {{here}}
trailingClosureSingle1 { 1 } // expected-error {{missing argument for parameter 'x' in call}} {{23-23=(x: <#T##Int#>)}}
trailingClosureSingle1() { 1 } // expected-error {{missing argument for parameter 'x' in call}} {{24-24=x: <#T##Int#>}}

func trailingClosureSingle2(x: () -> Int, y: Int) {} // expected-note * {{here}}
// FIXME: Bad diagnostics.
trailingClosureSingle2 { 1 } // expected-error {{missing argument for parameter 'x' in call}} {{23-23=(x: <#T##() -> Int#>)}}
trailingClosureSingle2() { 1 } // expected-error {{missing argument for parameter 'x' in call}} {{24-24=x: <#T##() -> Int#>}}

func trailingClosureMulti1(x: Int, y: Int, z: () -> Int) {} // expected-note * {{here}}
trailingClosureMulti1(y: 1) { 1 } // expected-error {{missing argument for parameter 'x' in call}} {{23-23=x: <#T##Int#>, }}
trailingClosureMulti1(x: 1) { 1 } // expected-error {{missing argument for parameter 'y' in call}} {{27-27=, y: <#T##Int#>}}
trailingClosureMulti1(x: 1, y: 1) // expected-error {{missing argument for parameter 'z' in call}} {{33-33=, z: <#T##() -> Int#>}}

func trailingClosureMulti2(x: Int, y: () -> Int, z: Int) {} // expected-note * {{here}}
trailingClosureMulti2 { 1 } // expected-error {{missing argument for parameter 'x' in call}} {{22-22=(x: <#T##Int#>)}}
// FIXME: Bad diagnostics.
trailingClosureMulti2() { 1 } // expected-error {{missing argument for parameter 'x' in call}} {{23-23=x: <#T##Int#>}}
trailingClosureMulti2(x: 1) { 1 } // expected-error {{missing argument for parameter 'y' in call}} {{27-27=, y: <#T##() -> Int#>}}

func param2Func(x: Int, y: Int) {} // expected-note * {{here}}
param2Func(x: 1) // expected-error {{missing argument for parameter 'y' in call}} {{16-16=, y: <#T##Int#>}}
param2Func(y: 1) // expected-error {{missing argument for parameter 'x' in call}} {{12-12=x: <#T##Int#>, }}

func param2FuncNonNamed1(_ x: Int, y: String) {} // expected-note * {{here}}
param2FuncNonNamed1(1) // expected-error {{missing argument for parameter 'y' in call}} {{22-22=, y: <#T##String#>}}
param2FuncNonNamed1(y: "foo") // expected-error {{missing argument for parameter #1 in call}} {{21-21=<#T##Int#>, }}

func param2FuncNonNamed2(x: Int, _ y: String) {} // expected-note * {{here}}
param2FuncNonNamed2(x: 1) // expected-error {{missing argument for parameter #2 in call}} {{25-25=, <#T##String#>}}
param2FuncNonNamed2("foo") // expected-error {{missing argument for parameter 'x' in call}} {{21-21=x: <#T##Int#>, }}

func param2FuncNonNamed3(_ x: Int, _ y: String) {} // expected-note * {{here}}
param2FuncNonNamed3(1) // expected-error {{missing argument for parameter #2 in call}} {{22-22=, <#T##String#>}}
param2FuncNonNamed3("foo") // expected-error {{missing argument for parameter #2 in call}} {{26-26=, <#T##String#>}}
                           // FIXME: Bad diagnostic. Could this be #1?

func param3Func(x: Int, y: Int, z: Int) {} // expected-note * {{here}}
param3Func(x: 1, y: 1) // expected-error {{missing argument for parameter 'z' in call}} {{22-22=, z: <#T##Int#>}}
param3Func(x: 1, z: 1) // expected-error {{missing argument for parameter 'y' in call}} {{16-16=, y: <#T##Int#>}}
param3Func(y: 1, z: 1) // expected-error {{missing argument for parameter 'x' in call}} {{12-12=x: <#T##Int#>, }}

func hasDefault1(x: Int, y: Int = 1) {} // expected-note * {{here}}
hasDefault1(x: 1) // ok
hasDefault1(y: 1) // expected-error {{missing argument for parameter 'x' in call}} {{13-13=x: <#T##Int#>, }}

func hasDefault2(x: Int = 1, y: Int) {} // expected-note * {{here}}
hasDefault2(x: 1) // expected-error {{missing argument for parameter 'y' in call}} {{17-17=, y: <#T##Int#>}}
hasDefault2(y: 1) // ok

struct S { init(x: Int, y: () -> Int) {} } // expected-note * {{here}}
_ = S(x: 1) // expected-error {{missing argument for parameter 'y' in call}} {{11-11=, y: <#T##() -> Int#>}}
_ = S(y: { 1 }) // expected-error {{missing argument for parameter 'x' in call}} {{7-7=x: <#T##Int#>, }}
_ = S { 1 } // expected-error {{missing argument for parameter 'x' in call}} {{6-6=(x: <#T##Int#>)}}
_ = S() { 1 } // expected-error {{missing argument for parameter 'x' in call}} {{7-7=x: <#T##Int#>}}

struct S1 {
  subscript(x x: Int, y y: () -> Int) -> Int { get { return 1 } set { } } // expected-note * {{here}}
}
var s1 = S1()
_ = s1[x: 1, y: {1}]  // Ok.
_ = s1[x: 1] { 1 } // Ok.
_ = s1[x: 1] // expected-error {{missing argument for parameter 'y' in call}} {{12-12=, y: <#T##() -> Int#>}}
_ = s1[y: { 1 }] // expected-error {{missing argument for parameter 'x' in call}} {{8-8=x: <#T##Int#>, }}
_ = s1[] { 1 } // expected-error {{cannot convert value of type '() -> Int' to expected argument type '(x: Int, y: () -> Int)'}} {{none}}
_ = s1 { 1 } // expected-error {{cannot call value of non-function type 'S1'}} {{none}}
_ = s1[] // expected-error {{missing argument for parameter 'x' in call}} {{8-8=x: <#T##Int#>}}
s1[x: 1, y: {1}] = 1 // Ok.
s1[x: 1] { 1 } = 1 // Ok.
s1[x: 1] = 1 // expected-error {{missing argument for parameter 'y' in call}} {{8-8=, y: <#T##() -> Int#>}}
s1[y: { 1 }] = 1 // expected-error {{missing argument for parameter 'x' in call}} {{4-4=x: <#T##Int#>, }}
s1[] { 1 } = 1 // expected-error {{cannot convert value of type '() -> Int' to expected argument type '(x: Int, y: () -> Int)'}} {{none}}

struct S2 {
  subscript(x: Int, y: () -> Int) -> Int { get { return 1 } set { } } // expected-note * {{here}}
}
var s2 = S2()
_ = s2[1, {1}]  // Ok.
_ = s2[1] { 1 } // Ok.
_ = s2[1] // expected-error {{cannot convert value of type 'Int' to expected argument type '(Int, () -> Int)'}} {{none}}
_ = s2[{ 1 }] // expected-error {{cannot convert value of type '() -> Int' to expected argument type '(Int, () -> Int)'}} {{none}}
_ = s2[] { 1 } // expected-error {{cannot convert value of type '() -> Int' to expected argument type '(Int, () -> Int)'}} {{none}}
_ = s2 { 1 } // expected-error {{cannot call value of non-function type 'S2'}} {{none}}
_ = s2[] // expected-error {{missing argument for parameter #1 in call}} {{8-8=<#T##Int#>}}
s2[1, {1}] = 1 // Ok.
s2[1] { 1 } = 1 // Ok.
s2[1] = 1 // expected-error {{cannot convert value of type 'Int' to expected argument type '(Int, () -> Int)'}} {{none}}
s2[{ 1 }] = 1 // expected-error {{cannot convert value of type '() -> Int' to expected argument type '(Int, () -> Int)'}} {{none}}
s2[] { 1 } = 1 // expected-error {{cannot convert value of type '() -> Int' to expected argument type '(Int, () -> Int)'}} {{none}}

struct S3 {
      subscript(x x: Int, y y: Int, z z: (Int) -> Int) -> Int { get { return 1 } set { } } // expected-note * {{here}}
}
var s3 = S3()
_ = s3[y: 1] { 1 } // expected-error {{missing argument for parameter 'x' in call}} {{8-8=x: <#T##Int#>, }}
_ = s3[x: 1] { 1 } // expected-error {{missing argument for parameter 'y' in call}} {{12-12=, y: <#T##Int#>}}
_ = s3[x: 1, y: 1] // expected-error {{missing argument for parameter 'z' in call}} {{18-18=, z: <#T##(Int) -> Int#>}}
s3[y: 1] { 1 } = 1 // expected-error {{missing argument for parameter 'x' in call}} {{4-4=x: <#T##Int#>, }}
s3[x: 1] { 1 } = 1 // expected-error {{missing argument for parameter 'y' in call}} {{8-8=, y: <#T##Int#>}}
s3[x: 1, y: 1] = 1 // expected-error {{missing argument for parameter 'z' in call}} {{14-14=, z: <#T##(Int) -> Int#>}}

struct S4 {
      subscript(x: Int, y: Int, z: (Int) -> Int) -> Int { get { return 1 } set { } } // expected-note * {{here}}
}
var s4 = S4()
_ = s4[1] { 1 } // expected-error {{missing argument for parameter #2 in call}} {{9-9=, <#T##Int#>}}
_ = s4[1, 1] // expected-error {{missing argument for parameter #3 in call}} {{12-12=, <#T##(Int) -> Int#>}}
s4[1] { 1 } = 1 // expected-error {{missing argument for parameter #2 in call}} {{5-5=, <#T##Int#>}}
s4[1, 1] = 1 // expected-error {{missing argument for parameter #3 in call}} {{8-8=, <#T##(Int) -> Int#>}}

func multiLine(x: Int, y: Int, z: Int) {} // expected-note * {{here}}
multiLine(
) // expected-error {{missing argument for parameter 'x' in call}} {{1-1=x: <#T##Int#>}}
multiLine(
  y: 1, // expected-error {{missing argument for parameter 'x' in call}} {{3-3=x: <#T##Int#>, }}
  z: 1
)
multiLine(
  x: 1, // expected-error {{missing argument for parameter 'y' in call}} {{7-7=, y: <#T##Int#>}}
  z: 1
)
multiLine(
  x: 1,
  y: 1 // expected-error {{missing argument for parameter 'z' in call}} {{7-7=, z: <#T##Int#>}}
)
