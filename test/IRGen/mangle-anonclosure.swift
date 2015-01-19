// RUN: %target-swift-frontend -emit-ir %s -disable-access-control -parse-stdlib -o - | FileCheck %s

import Swift

class HeapStorage<Value, Element> {
  public final func withUnsafeMutablePointerToElements<R>(
    body: (UnsafeMutablePointer<Element>)->R
  ) -> R { return body(UnsafeMutablePointer<Element>()) }
}
struct CountAndCapacity {}
class TestHeapStorage<T> : HeapStorage<CountAndCapacity,T> {
  deinit {
    withUnsafeMutablePointerToElements {
      // Don't crash when mangling this closure's name.
      // CHECK: _TFFC4main15TestHeapStoragedU_FGVSs20UnsafeMutablePointerQ__T_
      //         ---> main.TestHeapStorage.deinit.(closure #1)
      (p: UnsafeMutablePointer<T>)->() in
    }
  }
}
