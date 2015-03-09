// RUN: %swift-ide-test -structure -source-filename %s | FileCheck %s

// CHECK: <class>class <name>MyCls</name> : <inherited><elem-typeref>OtherClass</elem-typeref></inherited> {
// CHECK:   <property>var <name>bar</name> : Int</property>
// CHECK:   <property>var <name>anotherBar</name> : Int = 42</property>
// CHECK:   <cvar>class var <name>cbar</name> : Int = 0</cvar>
class MyCls : OtherClass {
  var bar : Int
  var anotherBar : Int = 42
  class var cbar : Int = 0

  // CHECK:   <ifunc>func <name>foo(<param>arg1</param>: Int, <param><name>name</name></param>: String, <param><name>param</name> par</param>: String)</name> {
  // CHECK:     var abc
  // CHECK:     <if>if <elem-condexpr>1</elem-condexpr> <brace>{
  // CHECK:       <call><name>foo</name>(<param>1</param>, <param><name>name</name>:</param>"test", <param><name>param</name>:</param>"test2")</call>
  // CHECK:     }</brace>
  // CHECK:   }</ifunc>
  func foo(arg1: Int, name: String, param par: String) {
    var abc
    if 1 {
      foo(1, name:"test", param:"test2")
    }
  }

  // CHECK:   <ifunc><name>init (<param><name>x</name></param>: Int)</name></ifunc>
  init (x: Int)

  // CHECK:   <cfunc>class func <name>cfoo()</name></cfunc>
  class func cfoo()

// CHECK: }</class>
}

// CHECK: <struct>struct <name>MyStruc</name> {
// CHECK:   <property>var <name>myVar</name>: Int</property>
// CHECK:   <svar>static var <name>sbar</name> : Int = 0</svar>
// CHECK:   <sfunc>static func <name>cfoo()</name></sfunc>
// CHECK: }</struct>
struct MyStruc {
  var myVar: Int
  static var sbar : Int = 0
  static func cfoo()
}

// CHECK: <protocol>protocol <name>MyProt</name> {
// CHECK:   <ifunc>func <name>foo()</name></ifunc>
// CHECK: }</protocol>
protocol MyProt {
  func foo()
}

// CHECK: <extension>extension <name>MyStruc</name> {
// CHECK:   <ifunc>func <name>foo()</name> {
// CHECK:   }</ifunc>
// CHECK: }</extension>
extension MyStruc {
  func foo() {
  }
}

// CHECK: <gvar>var <name>gvar</name> : Int = 0</gvar>
var gvar : Int = 0

// CHECK: <ffunc>func <name>ffoo()</name> {}</ffunc>
func ffoo() {}

// CHECK: <foreach>for <elem-id>i</elem-id> in <elem-expr>0...5</elem-expr> <brace>{}</brace></foreach>
for i in 0...5 {}
// CHECK: <for>for <elem-initexpr>var i = 0, i2 = 1</elem-initexpr>; <elem-expr>i == 0</elem-expr>; <elem-expr>++i</elem-expr> <brace>{}</brace></for>
for var i = 0, i2 = 1; i == 0; ++i {}
// CHECK: <for>for <elem-initexpr>i = 0</elem-initexpr>; <elem-expr>i == 0</elem-expr>; <elem-expr>++i</elem-expr> <brace>{}</brace></for>
for i = 0; i == 0; ++i {}
// CHECK: <while>while <elem-condexpr>var v = o, z = o where v > z</elem-condexpr> <brace>{}</brace></while>
while var v = o, z = o where v > z {}
// CHECK: <while>while <elem-condexpr>v == 0</elem-condexpr> <brace>{}</brace></while>
while v == 0 {}
// CHECK: <do-while>do <brace>{}</brace> while <elem-expr>v == 0</elem-expr></do-while>
do {} while v == 0
// CHECK: <if>if <elem-condexpr>var v = o, z = o where v > z</elem-condexpr> <brace>{}</brace></if>
if var v = o, z = o where v > z {}

// CHECK: <switch>switch <elem-expr>v</elem-expr> {
// CHECK:   <case>case <elem-pattern>1</elem-pattern>: break;</case>
// CHECK:   <case>case <elem-pattern>2</elem-pattern>, <elem-pattern>3</elem-pattern>: break;</case>
// CHECK:   <case>case <elem-pattern><call><name>Foo</name>(<param>var x</param>, <param>var y</param>)</call> where x < y</elem-pattern>: break;</case>
// CHECK:   <case>case <elem-pattern>2 where <call><name>foo</name>()</call></elem-pattern>, <elem-pattern>3 where <call><name>bar</name>()</call></elem-pattern>: break;</case>
// CHECK:   <case><elem-pattern>default</elem-pattern>: break;</case>
// CHECK: }</switch>
switch v {
  case 1: break;
  case 2, 3: break;
  case Foo(var x, var y) where x < y: break;
  case 2 where foo(), 3 where bar(): break;
  default: break;
}
