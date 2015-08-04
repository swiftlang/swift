// RUN: %swift-ide-test -structure -source-filename %s | FileCheck %s
// REQUIRES: object_literals

struct S: _ColorLiteralConvertible {
  init(colorLiteralRed: Float, green: Float, blue: Float, alpha: Float) {}
}

// CHECK: <gvar>let <name>y</name>: S = <object-literal-expression>[#<name>Color</name>(<param><name>colorLiteralRed</name>: </param>1, <param><name>green</name>: </param>0, <param><name>blue</name>: </param>0, <param><name>alpha</name>: </param>1)#]</object-literal-expression></gvar>
let y: S = [#Color(colorLiteralRed: 1, green: 0, blue: 0, alpha: 1)#]

struct I: _ImageLiteralConvertible {
  init?(imageLiteral: String) {}
}

// CHECK: <gvar>let <name>z</name>: I? = <object-literal-expression>[#<name>Image</name>(<param><name>imageLiteral</name>: </param>"hello.png")#]</object-literal-expression></gvar>
let z: I? = [#Image(imageLiteral: "hello.png")#]
