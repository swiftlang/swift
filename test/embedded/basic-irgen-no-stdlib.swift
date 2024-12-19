// RUN: %target-swift-emit-ir %s -parse-stdlib -enable-experimental-feature Embedded -target arm64e-apple-none -wmo | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

struct Bool {}

protocol Player {
  func play()
  var canPlay: Bool { get }
}

struct Concrete : Player {
  func play() { }
  var canPlay: Bool { Bool() }
}

func start(p: some Player) {
  p.play()
}

public func main() {
  start(p: Concrete())
}

// CHECK-LABEL: define {{.*}}void @"$e4main8ConcreteVACycfC"()

// CHECK-LABEL: define {{.*}}void @"$e4main5start1pyx_tAA6PlayerRzlFAA8ConcreteV_Tg5"()

// CHECK-LABEL: define {{.*}}void @"$e4mainAAyyF"()
// CHECK-NEXT:  entry:
// CHECK-NEXT:    call swiftcc void @"$e4main8ConcreteVACycfC"()
// CHECK-NEXT:    call swiftcc void @"$e4main5start1pyx_tAA6PlayerRzlFAA8ConcreteV_Tg5"()
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }
