// RUN: %target-typecheck-verify-swift

func nonNamedFunc(_ x: Int) {} // expected-note * {{here}}
nonNamedFunc() // expected-error {{missing argument for parameter #1 in call}} {{14-14=<#Int#>}}

func namedFunc(x: Int) {} // expected-note * {{here}}
namedFunc() // expected-error {{missing argument for parameter 'x' in call}} {{11-11=x: <#Int#>}}

func inoutFunc(x: inout Int) {} // expected-note * {{here}}
inoutFunc() // expected-error {{missing argument for parameter 'x' in call}} {{11-11=x: &<#Int#>}}

func autoclosureFunc(x: @autoclosure () -> Int) {} // expected-note * {{here}}
autoclosureFunc() // expected-error {{missing argument for parameter 'x' in call}} {{17-17=x: <#Int#>}}

func attributedFunc(x: @convention(c) () -> Int32) {} // expected-note * {{here}}
attributedFunc() // expected-error {{missing argument for parameter 'x' in call}} {{16-16=x: <#@convention(c) () -> Int32#>}}

func attributedInOutFunc(x: inout @convention(c) () -> Int32) {} // expected-note * {{here}}
attributedInOutFunc() // expected-error {{missing argument for parameter 'x' in call}} {{21-21=x: &<#@convention(c) () -> Int32#>}}

func genericFunc1<T>(x: T) {} // expected-note * {{here}}
genericFunc1() // expected-error {{missing argument for parameter 'x' in call}} {{14-14=x: <#T#>}}

protocol P {}
func genericFunc2<T : P>(x: T) {} // expected-note * {{here}}
genericFunc2() // expected-error {{missing argument for parameter 'x' in call}} {{14-14=x: <#T#>}}

typealias MyInt = Int
func aliasedFunc(x: MyInt) {} // expected-note * {{here}}
aliasedFunc() // expected-error {{missing argument for parameter 'x' in call}} {{13-13=x: <#MyInt#>}}

func trailingClosureSingle1(x: Int, y: () -> Int) {} // expected-note * {{here}}
trailingClosureSingle1 { 1 } // expected-error {{missing argument for parameter 'x' in call}} {{23-23=(x: <#Int#>)}}
trailingClosureSingle1() { 1 } // expected-error {{missing argument for parameter 'x' in call}} {{24-24=x: <#Int#>}}

func trailingClosureSingle2(x: () -> Int, y: Int) {} // expected-note * {{here}}
trailingClosureSingle2 { 1 }
// expected-error@-1 {{missing argument for parameter 'y' in call}} {{none}}
trailingClosureSingle2() { 1 }
// expected-error@-1 {{missing argument for parameter 'y' in call}} {{none}}

func trailingClosureMulti1(x: Int, y: Int, z: () -> Int) {} // expected-note * {{here}}
trailingClosureMulti1(y: 1) { 1 } // expected-error {{missing argument for parameter 'x' in call}} {{23-23=x: <#Int#>, }}
trailingClosureMulti1(x: 1) { 1 } // expected-error {{missing argument for parameter 'y' in call}} {{27-27=, y: <#Int#>}}
trailingClosureMulti1(x: 1, y: 1) // expected-error {{missing argument for parameter 'z' in call}} {{33-33=, z: <#() -> Int#>}}

func trailingClosureMulti2(x: Int, y: () -> Int, z: Int) {} // expected-note * {{here}}
trailingClosureMulti2 { 1 }
// expected-error@-1 {{missing arguments for parameters 'x', 'z' in call}}
trailingClosureMulti2() { 1 }
// expected-error@-1 {{missing arguments for parameters 'x', 'z' in call}}
trailingClosureMulti2(x: 1) { 1 }
// expected-error@-1 {{missing argument for parameter 'z' in call}} {{none}}

func trailingClosureMulti3(x: (Int) -> Int, y: (Int) -> Int, z: (Int) -> Int) {} // expected-note 2 {{declared here}}
trailingClosureMulti3 { $0 } y: { $0 } // expected-error{{missing argument for parameter 'z' in call}}{{39-39= z: <#(Int) -> Int#>}}

trailingClosureMulti3 { $0 } z: { $0 } // expected-error{{missing argument for parameter 'y' in call}}{{29-29= y: <#(Int) -> Int#>}}


func param2Func(x: Int, y: Int) {} // expected-note * {{here}}
param2Func(x: 1) // expected-error {{missing argument for parameter 'y' in call}} {{16-16=, y: <#Int#>}}
param2Func(y: 1) // expected-error {{missing argument for parameter 'x' in call}} {{12-12=x: <#Int#>, }}

func param2FuncNonNamed1(_ x: Int, y: String) {} // expected-note * {{here}}
param2FuncNonNamed1(1) // expected-error {{missing argument for parameter 'y' in call}} {{22-22=, y: <#String#>}}
param2FuncNonNamed1(y: "foo") // expected-error {{missing argument for parameter #1 in call}} {{21-21=<#Int#>, }}

func param2FuncNonNamed2(x: Int, _ y: String) {} // expected-note * {{here}}
param2FuncNonNamed2(x: 1) // expected-error {{missing argument for parameter #2 in call}} {{25-25=, <#String#>}}
param2FuncNonNamed2("foo") // expected-error {{missing argument for parameter 'x' in call}} {{21-21=x: <#Int#>, }}

func param2FuncNonNamed3(_ x: Int, _ y: String) {} // expected-note * {{here}}
param2FuncNonNamed3(1) // expected-error {{missing argument for parameter #2 in call}} {{22-22=, <#String#>}}
param2FuncNonNamed3("foo") // expected-error {{missing argument for parameter #1 in call}} {{21-21=<#Int#>, }}

func unlabeledParamFollowingVariadic(_: Any..., _: Any, _: Any) {} // expected-error {{a parameter following a variadic parameter requires a label}}; // expected-note {{here}}
unlabeledParamFollowingVariadic(1, 1, 1) // expected-error {{missing arguments for parameters #2, #3 in call}}

func labeledParamFollowingVariadic(_: Any..., label: Any, _: Any) {}
labeledParamFollowingVariadic(1, label: 1, 1)

func param3Func(x: Int, y: Int, z: Int) {} // expected-note * {{here}}
param3Func(x: 1, y: 1) // expected-error {{missing argument for parameter 'z' in call}} {{22-22=, z: <#Int#>}}
param3Func(x: 1, z: 1) // expected-error {{missing argument for parameter 'y' in call}} {{16-16=, y: <#Int#>}}
param3Func(y: 1, z: 1) // expected-error {{missing argument for parameter 'x' in call}} {{12-12=x: <#Int#>, }}

func hasDefault1(x: Int, y: Int = 1) {} // expected-note * {{here}}
hasDefault1(x: 1) // ok
hasDefault1(y: 1) // expected-error {{missing argument for parameter 'x' in call}} {{13-13=x: <#Int#>, }}

func hasDefault2(x: Int = 1, y: Int) {} // expected-note * {{here}}
hasDefault2(x: 1) // expected-error {{missing argument for parameter 'y' in call}} {{17-17=, y: <#Int#>}}
hasDefault2(y: 1) // ok

struct S { init(x: Int, y: () -> Int) {} } // expected-note * {{here}}
_ = S(x: 1) // expected-error {{missing argument for parameter 'y' in call}} {{11-11=, y: <#() -> Int#>}}
_ = S(y: { 1 }) // expected-error {{missing argument for parameter 'x' in call}} {{7-7=x: <#Int#>, }}
_ = S { 1 } // expected-error {{missing argument for parameter 'x' in call}} {{6-6=(x: <#Int#>)}}
_ = S() { 1 } // expected-error {{missing argument for parameter 'x' in call}} {{7-7=x: <#Int#>}}

struct S1 {
  subscript(x x: Int, y y: () -> Int) -> Int { get { return 1 } set { } } // expected-note * {{here}}
}
var s1 = S1()
_ = s1[x: 1, y: {1}]  // Ok.
_ = s1[x: 1] { 1 } // Ok.
_ = s1[x: 1] // expected-error {{missing argument for parameter 'y' in subscript}} {{12-12=, y: <#() -> Int#>}}
_ = s1[y: { 1 }] // expected-error {{missing argument for parameter 'x' in subscript}} {{8-8=x: <#Int#>, }}
_ = s1[] { 1 } // expected-error {{missing argument for parameter 'x' in subscript}} {{8-8=x: <#Int#>}}
_ = s1 { 1 } // expected-error {{cannot call value of non-function type 'S1'}} {{none}}
_ = s1[] // expected-error {{missing arguments for parameters 'x', 'y' in subscript}} {{8-8=x: <#Int#>, y: <#() -> Int#>}}
s1[x: 1, y: {1}] = 1 // Ok.
s1[x: 1] { 1 } = 1 // Ok.
s1[x: 1] = 1 // expected-error {{missing argument for parameter 'y' in subscript}} {{8-8=, y: <#() -> Int#>}}
s1[y: { 1 }] = 1 // expected-error {{missing argument for parameter 'x' in subscript}} {{4-4=x: <#Int#>, }}
s1[] { 1 } = 1 // expected-error {{missing argument for parameter 'x' in subscript}} {{4-4=x: <#Int#>}}

struct S2 {
  subscript(x: Int, y: () -> Int) -> Int { get { return 1 } set { } } // expected-note * {{here}}
}
var s2 = S2()
_ = s2[1, {1}]  // Ok.
_ = s2[1] { 1 } // Ok.
_ = s2[1] // expected-error {{missing argument for parameter #2 in subscript}} {{9-9=, <#() -> Int#>}}
_ = s2[{ 1 }] // expected-error {{missing argument for parameter #1 in subscript}} {{8-8=<#Int#>, }}
_ = s2[] { 1 } // expected-error {{missing argument for parameter #1 in subscript}} {{8-8=<#Int#>}}
_ = s2 { 1 } // expected-error {{cannot call value of non-function type 'S2'}} {{none}}
_ = s2[] // expected-error {{missing arguments for parameters #1, #2 in subscript}} {{8-8=<#Int#>, <#() -> Int#>}}
s2[1, {1}] = 1 // Ok.
s2[1] { 1 } = 1 // Ok.
s2[1] = 1 // expected-error {{missing argument for parameter #2 in subscript}} {{5-5=, <#() -> Int#>}}
s2[{ 1 }] = 1 // expected-error {{missing argument for parameter #1 in subscript}} {{4-4=<#Int#>, }}
s2[] { 1 } = 1 // expected-error {{missing argument for parameter #1 in subscript}} {{4-4=<#Int#>}}

struct S3 {
      subscript(x x: Int, y y: Int, z z: (Int) -> Int) -> Int { get { return 1 } set { } } // expected-note * {{here}}
}
var s3 = S3()
_ = s3[y: 1] { 1 } // expected-error {{missing argument for parameter 'x' in subscript}} {{8-8=x: <#Int#>, }}
// expected-error@-1 {{contextual type for closure argument list expects 1 argument, which cannot be implicitly ignored}} {{15-15= _ in}}
_ = s3[x: 1] { 1 } // expected-error {{missing argument for parameter 'y' in subscript}} {{12-12=, y: <#Int#>}}
// expected-error@-1 {{contextual type for closure argument list expects 1 argument, which cannot be implicitly ignored}} {{15-15= _ in}}
_ = s3[x: 1, y: 1] // expected-error {{missing argument for parameter 'z' in subscript}} {{18-18=, z: <#(Int) -> Int#>}}
s3[y: 1] { 1 } = 1 // expected-error {{missing argument for parameter 'x' in subscript}} {{4-4=x: <#Int#>, }}
// expected-error@-1 {{contextual type for closure argument list expects 1 argument, which cannot be implicitly ignored}} {{11-11= _ in}}
s3[x: 1] { 1 } = 1 // expected-error {{missing argument for parameter 'y' in subscript}} {{8-8=, y: <#Int#>}}
// expected-error@-1 {{contextual type for closure argument list expects 1 argument, which cannot be implicitly ignored}} {{11-11= _ in}}
s3[x: 1, y: 1] = 1 // expected-error {{missing argument for parameter 'z' in subscript}} {{14-14=, z: <#(Int) -> Int#>}}

struct S4 {
      subscript(x: Int, y: Int, z: (Int) -> Int) -> Int { get { return 1 } set { } } // expected-note * {{here}}
}
var s4 = S4()
_ = s4[1] { 1 } // expected-error {{missing argument for parameter #2 in subscript}} {{9-9=, <#Int#>}}
// expected-error@-1 {{contextual type for closure argument list expects 1 argument, which cannot be implicitly ignored}} {{12-12= _ in}}
_ = s4[1, 1] // expected-error {{missing argument for parameter #3 in subscript}} {{12-12=, <#(Int) -> Int#>}}
s4[1] { 1 } = 1 // expected-error {{missing argument for parameter #2 in subscript}} {{5-5=, <#Int#>}}
// expected-error@-1 {{contextual type for closure argument list expects 1 argument, which cannot be implicitly ignored}} {{8-8= _ in}}
s4[1, 1] = 1 // expected-error {{missing argument for parameter #3 in subscript}} {{8-8=, <#(Int) -> Int#>}}

func multiLine(x: Int, y: Int, z: Int) {} // expected-note * {{here}}
multiLine( // expected-error {{missing arguments for parameters 'x', 'y', 'z' in call}} {{11-11=x: <#Int#>, y: <#Int#>, z: <#Int#>}}
)
multiLine(
  y: 1, // expected-error {{missing argument for parameter 'x' in call}} {{3-3=x: <#Int#>, }}
  z: 1
)
multiLine(
  x: 1, // expected-error {{missing argument for parameter 'y' in call}} {{7-7=, y: <#Int#>}}
  z: 1
)
multiLine(
  x: 1,
  y: 1 // expected-error {{missing argument for parameter 'z' in call}} {{7-7=, z: <#Int#>}}
)
