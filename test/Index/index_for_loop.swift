// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s | %FileCheck %s

struct S<Element>: Sequence {
  struct Iterator: IteratorProtocol {
    func next() -> Element? { nil }
  }
  func makeIterator() -> Iterator { Iterator() }
}

func foo(_ xs: S<Int>) {
  for x in xs {}
  // CHECK: [[@LINE-1]]:3 | instance-method/Swift | makeIterator() | s:14swift_ide_test1SV12makeIteratorAC0E0Vyx_GyF
  // CHECK: [[@LINE-2]]:3 | instance-method/Swift | next() | s:14swift_ide_test1SV8IteratorV4nextxSgyF
}
