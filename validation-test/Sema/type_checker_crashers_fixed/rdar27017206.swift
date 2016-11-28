// RUN: %target-swift-frontend %s -parse
// REQUIRES: asserts

var str = "Hello"
String(str.characters.subscript(
    str.characters.startIndex..<str.characters.endIndex))
