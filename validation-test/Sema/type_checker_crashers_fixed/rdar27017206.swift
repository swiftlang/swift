// RUN: not %target-swift-frontend %s -typecheck
// REQUIRES: asserts

var str = "Hello"
String(str.characters.subscript(
    str.characters.startIndex..<str.characters.endIndex))
