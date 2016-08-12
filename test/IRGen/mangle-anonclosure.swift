// RUN: %target-swift-frontend -emit-ir %s -o - | %FileCheck %s

func someValidPointer<T>() -> UnsafeMutablePointer<T> { fatalError() }

class HeapStorage<Value, Element> {
  public final func withUnsafeMutablePointerToElements<R>(
    body: (UnsafeMutablePointer<Element>) -> R
  ) -> R { return body(someValidPointer()) }
}
struct CountAndCapacity {}
class TestHeapStorage<T> : HeapStorage<CountAndCapacity,T> {
  deinit {
    withUnsafeMutablePointerToElements {
      // Don't crash when mangling this closure's name.
      // CHECK: _TFFC4main15TestHeapStoragedU_FGSpQ__T_
      //         ---> main.TestHeapStorage.deinit.(closure #1)
      (p: UnsafeMutablePointer<T>) -> () in
    }
  }
}
