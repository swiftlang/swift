// REQUIRES: VENDOR=apple
// RUN: %target-swift-frontend -emit-ir -enable-testing -o/dev/null -module-name test -validate-tbd-against-ir=all %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -module-name test -validate-tbd-against-ir=all %s

protocol ProtoInternal {
  static var bar1: Self { get }
  static func bar2(arg: Int) -> Self
}

enum EnumInternal: ProtoInternal {
  case bar1
  case bar2(arg: Int)
}

public protocol ProtoPublic {
  static var bar3: Self { get }
  static func bar4(arg: Int) -> Self
}

public enum EnumPublic: ProtoPublic {
  case bar3
  case bar4(arg: Int)
}
