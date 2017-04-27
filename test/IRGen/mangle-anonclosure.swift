// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir %s -o - | %FileCheck %s

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
      // CHECK: _T04main15TestHeapStorageCfdySpyxGcfU_
      //         ---> main.TestHeapStorage.deinit.(closure #1)
      (p: UnsafeMutablePointer<T>) -> () in
    }
  }
}
