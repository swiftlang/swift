// RUN: %target-swift-frontend -import-objc-header %S/Inputs/0212-header.h -emit-sil %s

extension MyPoint {
  var x: Int { Int(y) }
}

public func printIt(point: MyPoint) {
  print(point)
}
