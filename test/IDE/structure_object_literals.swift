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
