// RUN: %target-parse-verify-swift
// REQUIRES: OS=macosx

struct S: _ColorLiteralConvertible {
  init(colorLiteralRed: Float, green: Float, blue: Float, alpha: Float) {}
}

let y: S = [#Color(colorLiteralRed: 1, green: 0, blue: 0, alpha: 1)#]
let y2 = [#Color(colorLiteralRed: 1, green: 0, blue: 0, alpha: 1)#] // expected-error{{could not infer type of color literal}} expected-note{{import AppKit to use 'NSColor' as the default color literal type}}
let y3 = [#Color(colorLiteralRed: 1, bleen: 0, grue: 0, alpha: 1)#] // expected-error{{cannot convert value of type '(colorLiteralRed: Int, bleen: Int, grue: Int, alpha: Int)' to expected argument type '(colorLiteralRed: Float, green: Float, blue: Float, alpha: Float)'}}

struct I: _ImageLiteralConvertible {
  init(imageLiteral: String) {}
}

let z: I = [#Image(imageLiteral: "hello.png")#]
let z2 = [#Image(imageLiteral: "hello.png")#] // expected-error{{could not infer type of image literal}} expected-note{{import AppKit to use 'NSImage' as the default image literal type}}

struct Path: _FileReferenceLiteralConvertible {
  init(fileReferenceLiteral: String) {}
}

let p1: Path = [#FileReference(fileReferenceLiteral: "what.txt")#]
let p2 = [#FileReference(fileReferenceLiteral: "what.txt")#] // expected-error{{could not infer type of file reference literal}} expected-note{{import Foundation to use 'NSURL' as the default file reference literal type}}
