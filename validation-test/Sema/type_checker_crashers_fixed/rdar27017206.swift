// RUN: not %target-swift-frontend %s -typecheck

var str = "Hello"
String(str.subscript(
    str.startIndex..<str.endIndex))
