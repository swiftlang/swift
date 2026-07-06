// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -swift-version 5 -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -parse-stdlib -enable-experimental-feature Embedded -wmo
// RUN: %target-swift-frontend -swift-version 5 -emit-ir     -I %t                      %t/Main.swift     -parse-stdlib -enable-experimental-feature Embedded -wmo | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// BEGIN MyModule.swift

public struct Bool {}

public protocol Player {
  func play()
  var canPlay: Bool { get }
}

public struct Concrete : Player {
  public init() { }
  public func play() { }
  public var canPlay: Bool { Bool() }
}

public func start(p: some Player) {
  p.play()
}

public func moduleMain() {
  start(p: Concrete())
}

// BEGIN Main.swift

import MyModule

public func main() {
  moduleMain()
}

// CHECK: define {{.*}}@{{_*}}main{{.*}} {
// CHECK: define {{.*}}void @"$e4Main4mainyyF"{{.*}} {
// CHECK: define {{.*}}void @"$e8MyModule10moduleMainyyF"{{.*}} {
// CHECK: define {{.*}}void @"$e8MyModule8ConcreteVACycfC"{{.*}} {
// CHECK: define {{.*}}void @"$e8MyModule5start1pyx_tAA6PlayerRzlFAA8ConcreteV_Tg5"{{.*}} {
// CHECK: define {{.*}}void @"$e8MyModule8ConcreteV4playyyF"{{.*}} {
