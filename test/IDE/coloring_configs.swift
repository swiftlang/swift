// RUN: %target-swift-ide-test -syntax-coloring -source-filename %s -D CONF | FileCheck %s

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
}
