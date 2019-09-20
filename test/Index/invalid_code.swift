// RUN: %target-swift-ide-test -print-indexed-symbols -include-locals -source-filename %s | %FileCheck %s

// CHECK: [[@LINE+1]]:8 | struct/Swift | Int | {{.*}} | Ref | rel: 0
var _: Int { get { return 1 } }

func test() {
  for o in allObjects {
    _ = o.something // don't crash
  }
}

class CrashTest {
  var something = 0
  func returnSelf(_ h: [AnyHashable: Any?]) -> CrashTest {
    return self
  }
  init() { }
}
// CHECK: [[@LINE+1]]:13 | instance-method/Swift | returnSelf
CrashTest().returnSelf(["": 0]).something()

class CrashTest2 {
// CHECK: [[@LINE+1]]:8 | instance-method/Swift | bar
  func bar() {
    someClosure { [weak self] in
      guard let sSelf = self else { return }

      let newDataProvider = Foo()
      newDataProvider.delegate = sSelf
    }
  }
}

public struct BadCollection: Collection {
    public var startIndex: Index { }
    public var endIndex: Index { }
    public func index(after index: Index) -> Index { }
    public subscript(position: Index) -> Element { }
}

struct Protector<T> {}
extension Protector where T: RangeReplaceableCollection {
  func append(_ newElement: T.Iterator.Element) {
    undefined { (foo: T) in
    // CHECK: [[@LINE-1]]:18 | param(local)/Swift | foo | {{.*}} | Def,RelChild
    // CHECK: [[@LINE-2]]:18 | function/acc-get(local)/Swift | getter:foo | {{.*}} | Def,Impl,RelChild,RelAcc
    // CHECK: [[@LINE-3]]:18 | function/acc-set(local)/Swift | setter:foo | {{.*}} | Def,Impl,RelChild,RelAcc
      _ = newElement
    }
  }
}