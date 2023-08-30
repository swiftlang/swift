// RUN: %target-swift-emit-ir %s -parse-stdlib -enable-experimental-feature Embedded | %FileCheck %s

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

// CHECK-LABEL: declare hidden swiftcc void @"$s4main8ConcreteVACycfC"()

// CHECK-LABEL: declare hidden swiftcc void @"$s4main5start1pyx_tAA6PlayerRzlFAA8ConcreteV_Tg5"()

// CHECK-LABEL: define swiftcc void @"$s4mainAAyyF"()
// CHECK-NEXT:  entry:
// CHECK-NEXT:    call swiftcc void @"$s4main8ConcreteVACycfC"()
// CHECK-NEXT:    call swiftcc void @"$s4main5start1pyx_tAA6PlayerRzlFAA8ConcreteV_Tg5"()
// CHECK-NEXT:    ret void
// CHECK-NEXT:  }
