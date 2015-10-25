// RUN: %target-parse-verify-swift

struct S: _ColorLiteralConvertible {
  init(colorLiteralRed: Float, green: Float, blue: Float, alpha: Float) {}
}

let y: S = [#Color(colorLiteralRed: 1, green: 0, blue: 0, alpha: 1)#]

struct I: _ImageLiteralConvertible {
  init(imageLiteral: String) {}
}

let z: I = [#Image(imageLiteral: "hello.png")#]

struct Path: _FileReferenceLiteralConvertible {
  init(fileReferenceLiteral: String) {}
}

let p1: Path = [#FileReference(fileReferenceLiteral: "what.txt")#]
