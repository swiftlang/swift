// RUN: %swift-ide-test -syntax-coloring -source-filename %s -D CONF | FileCheck %s

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

  // CHECK: <kw>var</kw> l : <type>Int</type>
  var l : Int
}
