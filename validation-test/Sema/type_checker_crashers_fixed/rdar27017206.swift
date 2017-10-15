// RUN: not %target-swift-frontend %s -typecheck

var str = "Hello"
String(str.characters.subscript(
    str.characters.startIndex..<str.characters.endIndex))
