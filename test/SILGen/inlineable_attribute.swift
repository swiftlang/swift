// RUN: %target-swift-frontend -emit-silgen -emit-verbose-sil %s | %FileCheck %s

// CHECK-LABEL: sil [fragile] @_TF20inlineable_attribute15fragileFunctionFT_T_ : $@convention(thin) () -> ()
@_inlineable public func fragileFunction() {

}

public struct MySt {
  // CHECK-LABEL: sil [fragile] @_TFV20inlineable_attribute4MySt6methodfT_T_ : $@convention(method) (MySt) -> ()
  @_inlineable public func method() {}

  // CHECK-LABEL: sil [fragile] @_TFV20inlineable_attribute4MyStg8propertySi : $@convention(method) (MySt) -> Int
  @_inlineable public var property: Int {
    return 5
  }

  // CHECK-LABEL: sil [fragile] @_TFV20inlineable_attribute4MyStg9subscriptFSiSi : $@convention(method) (Int, MySt) -> Int
  @_inlineable public subscript(x: Int) -> Int {
    return x
  }
}

public class MyCls {
  // CHECK-LABEL: sil [fragile] @_TFC20inlineable_attribute5MyClsD : $@convention(method) (@owned MyCls) -> ()
  @_inlineable deinit {}
}
