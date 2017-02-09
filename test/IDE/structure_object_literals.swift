// RUN: %swift-ide-test -structure -source-filename %s | %FileCheck %s

struct S: _ExpressibleByColorLiteral {
  init(colorLiteralRed: Float, green: Float, blue: Float, alpha: Float) {}
}

// CHECK: <gvar>let <name>y</name>: S = <object-literal-expression>#<name>colorLiteral</name>(<arg><name>red</name>: 1</arg>, <arg><name>green</name>: 0</arg>, <arg><name>blue</name>: 0</arg>, <arg><name>alpha</name>: 1</arg>)</object-literal-expression></gvar>
let y: S = #colorLiteral(red: 1, green: 0, blue: 0, alpha: 1)

struct I: _ExpressibleByImageLiteral {
  init?(imageLiteralResourceName: String) {}
}

// CHECK: <gvar>let <name>z</name>: I? = <object-literal-expression>#<name>imageLiteral</name>(<arg><name>resourceName</name>: "hello.png"</arg>)</object-literal-expression></gvar>
let z: I? = #imageLiteral(resourceName: "hello.png")

func before() {}
// CHECK-AFTER: <ffunc>func <name>before()</name> {}</ffunc>
_ = .<#line#>
_ = 1<#line
_ = 1<#line#>
_ = a.+<#file#>
_ = x.<; <# #>;
_ = 2.<#file(2)
<##>
x <##> y
_ = .<#placeholder#>
_ = #colorLiteral(red: 0.0, green: 0.0, blue: 0.0, alpha: 1.0)

.<# place #> #fileLiteral(resourceName: "sdfds") .<# holder #>
.<##>#imageLiteral(resourceName: <# name #>)x.<##>

func after() {}
// CHECK-AFTER: _ = <object-literal-expression>#<name>colorLiteral</name>(<arg><name>red</name>: 0.0</arg>, <arg><name>green</name>: 0.0</arg>, <arg><name>blue</name>: 0.0</arg>, <arg><name>alpha</name>: 1.0</arg>)</object-literal-expression>
// CHECK-AFTER: .<# place #> <object-literal-expression>#<name>fileLiteral</name>(<arg><name>resourceName</name>: "sdfds"</arg>)</object-literal-expression> .<# holder #>
// CHECK-AFTER: .<##><object-literal-expression>#<name>imageLiteral</name>(<arg><name>resourceName</name>: <# name #></arg>)</object-literal-expression>x.<##>
// CHECK-AFTER: <ffunc>func <name>after()</name> {}</ffunc>

