// RUN: %target-swift-frontend -enable-builtin-module -O -Xllvm -sil-print-types -emit-sil %s | %IRGenFileCheck %s

import Builtin

// CHECK-LABEL: sil @storeBytesBitwiseCopyable : {{.*}} {
// CHECK:       bb0(
// CHECK-SAME:      [[SELF:%[^,]+]] : $UnsafeMutableRawPointer, 
// CHECK-SAME:      [[VALUE:%[^,]+]] : $*T, 
// CHECK-SAME:      [[OFFSET:%[^,]+]] : $Int, 
// CHECK-SAME:      {{%[^,]+}} : $@thick T.Type):
// CHECK:         [[SELF_RAW_VALUE:%[^,]+]] = struct_extract [[SELF]] : {{.*}} #UnsafeMutableRawPointer._rawValue
// CHECK:         [[OFFSET_VALUE:%[^,]+]] = struct_extract [[OFFSET]] : {{.*}} #Int._value
// CHECK:         [[OFFSET_WORD:%[^,]+]] = builtin "{{trunc|sext}}OrBitCast_Int[[PTR_SIZE]]_Word"([[OFFSET_VALUE]] : $Builtin.Int[[PTR_SIZE]])
// CHECK:         [[DESTINATION_POINTER:%[^,]+]] = index_raw_pointer [[SELF_RAW_VALUE]] : $Builtin.RawPointer, [[OFFSET_WORD]]
// CHECK:         [[DESTINATION:%[^,]+]] = pointer_to_address [[DESTINATION_POINTER]] : $Builtin.RawPointer to [align=1]
// CHECK:         copy_addr [[VALUE]] to [[DESTINATION]]
// CHECK-LABEL: } // end sil function 'storeBytesBitwiseCopyable'
@_silgen_name("storeBytesBitwiseCopyable")
public func storeBytes<T : BitwiseCopyable>(
  _ self: UnsafeMutableRawPointer,
  of value: T, toByteOffset offset: Int = 0, as type: T.Type
) {
  Builtin.storeRaw(value, (self + offset)._rawValue)
}
