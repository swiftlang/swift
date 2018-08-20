// We need to require macOS because swiftSyntax currently doesn't build on Linux
// REQUIRES: OS=macosx
// RUN: %target-swift-ide-test -syntax-coloring -source-filename %s -D CONF | %FileCheck %s
// RUN: %swift-swiftsyntax-test -classify-syntax -source-file %s | %FileCheck %s

// CHECK: <kw>var</kw> f : <type>Int</type>
var f : Int

// CHECK: <#kw>#if</#kw> <#id>os</#id>(<#id>macOS</#id>)
#if os(macOS)
#endif

// CHECK: <#kw>#if</#kw> <#id>CONF</#id>
#if CONF
  // CHECK: <kw>var</kw> x : <type>Int</type>
  var x : Int
// CHECK: <#kw>#else</#kw>
#else
  // CHECK: <kw>var</kw> x : <type>Float</type>
  var x : Float
// CHECK: <#kw>#endif</#kw>
#endif

// CHECK: <#kw>#if</#kw> <#id>CONF</#id>
#if CONF
  // CHECK: <kw>var</kw> x2 : <type>Int</type>
  var x2 : Int
// CHECK: <#kw>#endif</#kw>
#endif

// CHECK: <#kw>#if</#kw> !<#id>CONF</#id>
#if !CONF
  // CHECK: <kw>var</kw> x3 : <type>Int</type>
  var x3 : Int
// CHECK: <#kw>#else</#kw>
#else
  // CHECK: <kw>var</kw> x3 : <type>Float</type>
  var x3 : Float
// CHECK: <#kw>#endif</#kw>
#endif

// CHECK: <#kw>#if</#kw> !<#id>CONF</#id>
#if !CONF
  // CHECK: <kw>var</kw> x4 : <type>Int</type>
  var x4 : Int
// CHECK: <#kw>#endif</#kw>
#endif

// CHECK: <#kw>#if</#kw> <#id>CONF</#id>
#if CONF
  // CHECK: <kw>var</kw> y1 : <type>Int</type>
  var y1 : Int
// CHECK: <#kw>#elseif</#kw> <#id>BAZ</#id>
#elseif BAZ
  // CHECK: <kw>var</kw> y1 : <type>String</type>
  var y1 : String
// CHECK: <#kw>#else</#kw>
#else
  // CHECK: <kw>var</kw> y1 : <type>Float</type>
  var y1 : Float
// CHECK: <#kw>#endif</#kw>
#endif

// CHECK: <#kw>#if</#kw> !<#id>CONF</#id>
#if !CONF
  // CHECK: <kw>var</kw> y2 : <type>Int</type>
  var y2 : Int
// CHECK: <#kw>#elseif</#kw> <#id>BAZ</#id>
#elseif BAZ
  // CHECK: <kw>var</kw> y2 : <type>String</type>
  var y2 : String
// CHECK: <#kw>#else</#kw>
#else
  // CHECK: <kw>var</kw> y2 : <type>Float</type>
  var y2 : Float
// CHECK: <#kw>#endif</#kw>
#endif

// CHECK: <#kw>#if</#kw> !<#id>CONF</#id>
#if !CONF
  // CHECK: <kw>var</kw> y3 : <type>Int</type>
  var y3 : Int
// CHECK: <#kw>#elseif</#kw> <#id>CONF</#id>
#elseif CONF
  // CHECK: <kw>var</kw> y3 : <type>String</type>
  var y3 : String
// CHECK: <#kw>#else</#kw>
#else
  // CHECK: <kw>var</kw> y3 : <type>Float</type>
  var y3 : Float
// CHECK: <#kw>#endif</#kw>
#endif

// CHECK: <kw>var</kw> l : <type>Int</type>
var l : Int

// CHECK: <kw>class</kw> C1 {
class C1 {
  // CHECK: <kw>var</kw> f : <type>Int</type>
  var f : Int

// CHECK: <#kw>#if</#kw> <#id>CONF</#id>
#if CONF
  // CHECK: <kw>var</kw> x : <type>Int</type>
  var x : Int
// CHECK: <#kw>#else</#kw>
#else
  // CHECK: <kw>var</kw> x : <type>Float</type>
  var x : Float
// CHECK: <#kw>#endif</#kw>
#endif

// CHECK: <#kw>#if</#kw> <#id>CONF</#id>
#if CONF
  // CHECK: <kw>var</kw> x2 : <type>Int</type>
  var x2 : Int
// CHECK: <#kw>#endif</#kw>
#endif

// CHECK: <#kw>#if</#kw> !<#id>CONF</#id>
#if !CONF
  // CHECK: <kw>var</kw> x3 : <type>Int</type>
  var x3 : Int
// CHECK: <#kw>#else</#kw>
#else
  // CHECK: <kw>var</kw> x3 : <type>Float</type>
  var x3 : Float
// CHECK: <#kw>#endif</#kw>
#endif

// CHECK: <#kw>#if</#kw> !<#id>CONF</#id>
#if !CONF
  // CHECK: <kw>var</kw> x4 : <type>Int</type>
  var x4 : Int
// CHECK: <#kw>#endif</#kw>
#endif

// CHECK: <#kw>#if</#kw> <#id>CONF</#id>
#if CONF
  // CHECK: <kw>var</kw> y1 : <type>Int</type>
  var y1 : Int
// CHECK: <#kw>#elseif</#kw> <#id>BAZ</#id>
#elseif BAZ
  // CHECK: <kw>var</kw> y1 : <type>String</type>
  var y1 : String
// CHECK: <#kw>#else</#kw>
#else
  // CHECK: <kw>var</kw> y1 : <type>Float</type>
  var y1 : Float
// CHECK: <#kw>#endif</#kw>
#endif

// CHECK: <#kw>#if</#kw> !<#id>CONF</#id>
#if !CONF
  // CHECK: <kw>var</kw> y2 : <type>Int</type>
  var y2 : Int
// CHECK: <#kw>#elseif</#kw> <#id>BAZ</#id>
#elseif BAZ
  // CHECK: <kw>var</kw> y2 : <type>String</type>
  var y2 : String
// CHECK: <#kw>#else</#kw>
#else
  // CHECK: <kw>var</kw> y2 : <type>Float</type>
  var y2 : Float
// CHECK: <#kw>#endif</#kw>
#endif

// CHECK: <#kw>#if</#kw> !<#id>CONF</#id>
#if !CONF
  // CHECK: <kw>var</kw> y3 : <type>Int</type>
  var y3 : Int
// CHECK: <#kw>#elseif</#kw> <#id>CONF</#id>
#elseif CONF
  // CHECK: <kw>var</kw> y3 : <type>String</type>
  var y3 : String
// CHECK: <#kw>#else</#kw>
#else
  // CHECK: <kw>var</kw> y3 : <type>Float</type>
  var y3 : Float
// CHECK: <#kw>#endif</#kw>
#endif

  // CHECK: <kw>var</kw> l : <type>Int</type>
  var l : Int
}

// CHECK: <kw>func</kw> test1() {
func test1() {
  // CHECK: <kw>var</kw> f : <type>Int</type>
  var f : Int

// CHECK: <#kw>#if</#kw> <#id>CONF</#id>
#if CONF
  // CHECK: <kw>var</kw> x : <type>Int</type>
  var x : Int
// CHECK: <#kw>#else</#kw>
#else
  // CHECK: <kw>var</kw> x : <type>Float</type>
  var x : Float
// CHECK: <#kw>#endif</#kw>
#endif

// CHECK: <#kw>#if</#kw> <#id>CONF</#id>
#if CONF
  // CHECK: <kw>var</kw> x2 : <type>Int</type>
  var x2 : Int
// CHECK: <#kw>#endif</#kw>
#endif

// CHECK: <#kw>#if</#kw> !<#id>CONF</#id>
#if !CONF
  // CHECK: <kw>var</kw> x3 : <type>Int</type>
  var x3 : Int
// CHECK: <#kw>#else</#kw>
#else
  // CHECK: <kw>var</kw> x3 : <type>Float</type>
  var x3 : Float
// CHECK: <#kw>#endif</#kw>
#endif

// CHECK: <#kw>#if</#kw> !<#id>CONF</#id>
#if !CONF
  // CHECK: <kw>var</kw> x4 : <type>Int</type>
  var x4 : Int
// CHECK: <#kw>#endif</#kw>
#endif

// CHECK: <#kw>#if</#kw> <#id>CONF</#id>
#if CONF
  // CHECK: <kw>var</kw> y1 : <type>Int</type>
  var y1 : Int
// CHECK: <#kw>#elseif</#kw> <#id>BAZ</#id>
#elseif BAZ
  // CHECK: <kw>var</kw> y1 : <type>String</type>
  var y1 : String
// CHECK: <#kw>#else</#kw>
#else
  // CHECK: <kw>var</kw> y1 : <type>Float</type>
  var y1 : Float
// CHECK: <#kw>#endif</#kw>
#endif

// CHECK: <#kw>#if</#kw> !<#id>CONF</#id>
#if !CONF
  // CHECK: <kw>var</kw> y2 : <type>Int</type>
  var y2 : Int
// CHECK: <#kw>#elseif</#kw> <#id>BAZ</#id>
#elseif BAZ
  // CHECK: <kw>var</kw> y2 : <type>String</type>
  var y2 : String
// CHECK: <#kw>#else</#kw>
#else
  // CHECK: <kw>var</kw> y2 : <type>Float</type>
  var y2 : Float
// CHECK: <#kw>#endif</#kw>
#endif

// CHECK: <#kw>#if</#kw> !<#id>CONF</#id>
#if !CONF
  // CHECK: <kw>var</kw> y3 : <type>Int</type>
  var y3 : Int
// CHECK: <#kw>#elseif</#kw> <#id>CONF</#id>
#elseif CONF
  // CHECK: <kw>var</kw> y3 : <type>String</type>
  var y3 : String
// CHECK: <#kw>#else</#kw>
#else
  // CHECK: <kw>var</kw> y3 : <type>Float</type>
  var y3 : Float
// CHECK: <#kw>#endif</#kw>
#endif

  // CHECK: <kw>var</kw> l : <type>Int</type>
  var l : Int
}

// CHECK: <kw>class</kw> C2 {
class C2 {
  // CHECK: <#kw>#if</#kw> <#id>os</#id>(<#id>iOS</#id>)
  #if os(iOS)
  // CHECK: <kw>func</kw> foo() {}
  func foo() {}
  #endif
}

class NestedPoundIf {
// CHECK: <kw>class</kw> NestedPoundIf {
    func foo1() {
// CHECK: <kw>func</kw> foo1() {
        #if os(macOS)
// CHECK: <#kw>#if</#kw> <#id>os</#id>(<#id>macOS</#id>)
          var a = 1
// CHECK: <kw>var</kw> a = <int>1</int>
            #if USE_METAL
// CHECK: <#kw>#if</#kw> <#id>USE_METAL</#id>
              var b = 2
// CHECK: <kw>var</kw> b = <int>2</int>
              #if os(iOS)
// CHECK: <#kw>#if</#kw> <#id>os</#id>(<#id>iOS</#id>)
                var c = 3
// CHECK: <kw>var</kw> c = <int>3</int>
              #else
// CHECK: <#kw>#else</#kw>
                var c = 3
// CHECK: <kw>var</kw> c = <int>3</int>
              #endif
// CHECK: <#kw>#endif</#kw>
            #else
// CHECK: <#kw>#else</#kw>
              var b = 2
// CHECK: <kw>var</kw> b = <int>2</int>
            #endif
// CHECK: <#kw>#endif</#kw>
           #else
// CHECK: <#kw>#else</#kw>
            var a = 1
// CHECK: <kw>var</kw> a = <int>1</int>
        #endif
// CHECK: <#kw>#endif</#kw>
    }
    func foo2() {}
// CHECK: <kw>func</kw> foo2() {}
    func foo3() {}
// CHECK: <kw>func</kw> foo3() {}
}

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
