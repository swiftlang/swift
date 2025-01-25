// RUN: %target-swift-ide-test -syntax-coloring -source-filename %s -D CONF | %FileCheck %s

// CHECK: <#kw>#error</#kw>(<str>"Error"</str>)
#error("Error")
// CHECK: <#kw>#warning</#kw>(<str>"Warning"</str>)
#warning("Warning")
// CHECK: <#kw>#sourceLocation</#kw>(file: <str>"x"</str>, line: <int>1</int>)
#sourceLocation(file: "x", line: 1)
// CHECK: <kw>#line</kw> <int>17</int> <str>"abc.swift"</str>
#line 17 "abc.swift"

@available(iOS 8.0, OSX 10.10, *)
// CHECK: <attr-builtin>@available</attr-builtin>(<kw>iOS</kw> <float>8.0</float>, <kw>OSX</kw> <float>10.10</float>, *)
func foo() {
// CHECK: <kw>if</kw> <kw>#available</kw> (<kw>OSX</kw> <float>10.10</float>, <kw>iOS</kw> <float>8.01</float>, *) {<kw>let</kw> <kw>_</kw> = <str>"iOS"</str>}
  if #available (OSX 10.10, iOS 8.01, *) {let _ = "iOS"}
}

class AvailableWithOverride {
  // CHECK: <attr-builtin>@available</attr-builtin>(<kw>iOS</kw> <float>8.01</float>, <kw>OSX</kw> <float>10.10</float>, *)
  @available(iOS 8.01, OSX 10.10, *)
  // CHECK: <attr-builtin>public</attr-builtin> <attr-builtin>override</attr-builtin> <kw>var</kw> multiple: <type>Int</type> { <kw>return</kw> <int>24</int> }
  public override var multiple: Int { return 24 }
}

// CHECK: <kw>func</kw> test4(<kw>inout</kw> a: <type>Int</type>) {{{$}}
func test4(inout a: Int) {
  // CHECK-OLD: <kw>if</kw> <kw>#available</kw> (<kw>OSX</kw> >= <float>10.10</float>, <kw>iOS</kw> >= <float>8.01</float>) {<kw>let</kw> OSX = <str>"iOS"</str>}}{{$}}
  // CHECK-NEW: <kw>if</kw> <kw>#available</kw> (OSX >= <float>10.10</float>, iOS >= <float>8.01</float>) {<kw>let</kw> OSX = <str>"iOS"</str>}}{{$}}
  if #available (OSX >= 10.10, iOS >= 8.01) {let OSX = "iOS"}}

// CHECK: <kw>func</kw> test4b(a: <kw>inout</kw> <type>Int</type>) {{{$}}
func test4b(a: inout Int) {
}

let filename = #file
// CHECK: <kw>let</kw> filename = <kw>#file</kw>
let line = #line
// CHECK: <kw>let</kw> line = <kw>#line</kw>
let column = #column
// CHECK: <kw>let</kw> column = <kw>#column</kw>
let function = #function
// CHECK: <kw>let</kw> function = <kw>#function</kw>

let image = #imageLiteral(resourceName: "cloud.png")
// CHECK-OLD: <kw>let</kw> image = <object-literal>#imageLiteral(resourceName: "cloud.png")</object-literal>
// CHECK-NEW: <kw>let</kw> image = <object-literal>#imageLiteral</object-literal>(resourceName: <str>"cloud.png"</str>)
let file = #fileLiteral(resourceName: "cloud.png")
// CHECK-OLD: <kw>let</kw> file = <object-literal>#fileLiteral(resourceName: "cloud.png")</object-literal>
// CHECK-NEW: <kw>let</kw> file = <object-literal>#fileLiteral</object-literal>(resourceName: <str>"cloud.png"</str>)
let black = #colorLiteral(red: 0, green: 0, blue: 0, alpha: 1)
// CHECK-OLD: <kw>let</kw> black = <object-literal>#colorLiteral(red: 0, green: 0, blue: 0, alpha: 1)</object-literal>
// CHECK-NEW: <kw>let</kw> black = <object-literal>#colorLiteral</object-literal>(red: <int>0</int>, green: <int>0</int>, blue: <int>0</int>, alpha: <int>1</int>)

let rgb = [#colorLiteral(red: 1, green: 0, blue: 0, alpha: 1),
           #colorLiteral(red: 0, green: 1, blue: 0, alpha: 1),
           #colorLiteral(red: 0, green: 0, blue: 1, alpha: 1)]
// CHECK-OLD: <kw>let</kw> rgb = [<object-literal>#colorLiteral(red: 1, green: 0, blue: 0, alpha: 1)</object-literal>,
// CHECK-OLD:                     <object-literal>#colorLiteral(red: 0, green: 1, blue: 0, alpha: 1)</object-literal>,
// CHECK-OLD:                     <object-literal>#colorLiteral(red: 0, green: 0, blue: 1, alpha: 1)</object-literal>]
// CHECK-NEW: <kw>let</kw> rgb = [<object-literal>#colorLiteral</object-literal>(red: <int>1</int>, green: <int>0</int>, blue: <int>0</int>, alpha: <int>1</int>),
// CHECK-NEW:                     <object-literal>#colorLiteral</object-literal>(red: <int>0</int>, green: <int>1</int>, blue: <int>0</int>, alpha: <int>1</int>),
// CHECK-NEW:                     <object-literal>#colorLiteral</object-literal>(red: <int>0</int>, green: <int>0</int>, blue: <int>1</int>, alpha: <int>1</int>)]
