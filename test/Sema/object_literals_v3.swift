// RUN: %target-typecheck-verify-swift -swift-version 3

struct Path: _ExpressibleByFileReferenceLiteral {
  init(fileReferenceLiteralResourceName: String) {}
}

let p1: Path = #fileLiteral(resourceName: "what.txt")
let string = "what.txt"
let p3: Path = #fileLiteral(resourceName: string) // expected-warning{{non-literal or interpolated literal arguments in object literals are deprecated}}
let p4: Path = #fileLiteral(resourceName: "\(string)") // expected-warning{{non-literal or interpolated literal arguments in object literals are deprecated}}
