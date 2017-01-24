// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen -emit-verbose-sil %s | %FileCheck %s

// CHECK-LABEL: sil [fragile] @_T020inlineable_attribute15fragileFunctionyyF : $@convention(thin) () -> ()
@_inlineable public func fragileFunction() {

}

public struct MySt {
  // CHECK-LABEL: sil [fragile] @_T020inlineable_attribute4MyStV6methodyyF : $@convention(method) (MySt) -> ()
  @_inlineable public func method() {}

  // CHECK-LABEL: sil [fragile] @_T020inlineable_attribute4MyStV8propertySifg : $@convention(method) (MySt) -> Int
  @_inlineable public var property: Int {
    return 5
  }

  // CHECK-LABEL: sil [fragile] @_T020inlineable_attribute4MyStV9subscriptSiSicfg : $@convention(method) (Int, MySt) -> Int
  @_inlineable public subscript(x: Int) -> Int {
    return x
  }
}

public class MyCls {
  // CHECK-LABEL: sil [fragile] @_T020inlineable_attribute5MyClsCfD : $@convention(method) (@owned MyCls) -> ()
  @_inlineable deinit {}
}
