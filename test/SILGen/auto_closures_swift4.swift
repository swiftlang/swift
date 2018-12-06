
// RUN: %target-swift-emit-silgen -module-name auto_closures -enable-sil-ownership -parse-stdlib -swift-version 4 %s

// Swift 4-style autoclosure forwarding should not crash - rdar://problem/44657505

public struct Empty {}

public func x(_: @autoclosure () -> Empty) {}

public func y1(_ message: @autoclosure () -> Empty) {
  x(message)
}

public func y2(_ message: @autoclosure @escaping () -> Empty) {
  x(message)
}
