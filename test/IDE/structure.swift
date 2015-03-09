// RUN: %swift-ide-test -structure -source-filename %s | FileCheck %s

// CHECK: <class>class <name>MyCls</name> : <inherited>OtherClass</inherited> {
// CHECK:   <property>var <name>bar</name> : Int</property>
// CHECK:   <property>var <name>anotherBar</name> : Int = 42</property>
// CHECK:   <cvar>class var <name>cbar</name> : Int = 0</cvar>
class MyCls : OtherClass {
  var bar : Int
  var anotherBar : Int = 42
  class var cbar : Int = 0

  // CHECK:   <ifunc>func <name>foo(<param>arg1</param>: Int, <param><name>name</name></param>: String, <param><name>param</name> par</param>: String)</name> {
  // CHECK:     var abc
  // CHECK:     if 1 <brace>{
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
