// RUN: %target-parse-verify-swift -debug-constraints

struct S: _ColorLiteralConvertible {
  init(colorLiteralRed: Float, green: Float, blue: Float, alpha: Float) {}
}

let y: S = [#Color(colorLiteralRed: 1, green: 0, blue: 0, alpha: 1)#]

struct I: _ImageLiteralConvertible {
  init?(imageLiteral: String) {}
}

let z: I? = [#Image(imageLiteral: "hello.png")#]
let z2: I = [#Image(imageLiteral: "hello2.png")#] // expected-error{{value of optional type 'I?' not unwrapped; did you mean to use '!' or '?'?}}
