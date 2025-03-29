// RUN: %target-swift-frontend -O -Xllvm -sil-print-types -emit-sil -disable-availability-checking %s -enable-builtin-module -enable-experimental-feature RawLayout | %FileCheck %s

// REQUIRES: swift_feature_RawLayout

import Builtin

@frozen
@_rawLayout(like: T)
public struct Cell<T: ~Copyable>: ~Copyable {
  // CHECK-LABEL: sil {{.*}} @$s4CellAAVAARi_zrlE7addressSpyxGvg : $@convention(method) <T where T : ~Copyable> (@in_guaranteed Cell<T>) -> UnsafeMutablePointer<T> {
  // CHECK:       bb0([[SELF:%.*]] : $*Cell<T>):
  // CHECK:         [[RAW_LAYOUT_ADDR:%.*]] = builtin "addressOfRawLayout"<Cell<T>>([[SELF]] : $*Cell<T>) : $Builtin.RawPointer
  // CHECK-NEXT:    [[POINTER:%.*]] = struct $UnsafeMutablePointer<T> ([[RAW_LAYOUT_ADDR]] : $Builtin.RawPointer)
  // CHECK-NEXT:    return [[POINTER]] : $UnsafeMutablePointer<T>
  // CHECK-LABEL: } // end sil function '$s4CellAAVAARi_zrlE7addressSpyxGvg'
  @_transparent
  public var address: UnsafeMutablePointer<T> {
    .init(Builtin.addressOfRawLayout(self))
  }

  // CHECK-LABEL: sil {{.*}} @$s4CellAAVAARi_zrlEyAByxGxcfC : $@convention(method) <T where T : ~Copyable> (@in T, @thin Cell<T>.Type) -> @out Cell<T> {
  // CHECK:       bb0({{%.*}} : $*Cell<T>, [[VALUE:%.*]] : $*T, {{%.*}} : $@thin Cell<T>.Type):
  // CHECK:         {{%.*}} = builtin "zeroInitializer"([[SELF:%.*]] : $*Cell<T>) : $()
  // CHECK-NEXT:    [[RAW_LAYOUT_ADDR:%.*]] = builtin "addressOfRawLayout"<Cell<T>>([[SELF]] : $*Cell<T>) : $Builtin.RawPointer
  // CHECK-NEXT:    [[POINTER:%.*]] = struct $UnsafeMutablePointer<T> ([[RAW_LAYOUT_ADDR]] : $Builtin.RawPointer)
  //                Calling 'UnsafeMutablePointer<T>.initialize(to:)'
  // CHECK:         {{%.*}} = apply {{%.*}}<T>([[VALUE]], [[POINTER]])
  // CHECK-LABEL: } // end sil function '$s4CellAAVAARi_zrlEyAByxGxcfC'
  @_transparent
  public init(_ value: consuming T) {
    address.initialize(to: value)
  }

  @inlinable
  deinit {
    address.deinitialize(count: 1)
  }
}
