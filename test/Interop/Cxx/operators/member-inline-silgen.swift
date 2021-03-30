// RUN: %target-swift-emit-sil %s -I %S/Inputs -enable-cxx-interop | %FileCheck %s

import MemberInline

public func sub(_ lhs: inout LoadableIntWrapper, _ rhs: LoadableIntWrapper) -> LoadableIntWrapper { lhs - rhs }

// CHECK: bb0([[SELF:%.*]] : $*LoadableIntWrapper, [[RHS:%.*]] : $LoadableIntWrapper):

// CHECK: [[SELFACCESS:%.*]] = begin_access [modify] [static] [[SELF]] : $*LoadableIntWrapper
// CHECK: [[OP:%.*]] = function_ref [[NAME:@(_ZN18LoadableIntWrappermiES_|\?\?GLoadableIntWrapper@@QEAA\?AU0@U0@@Z)]] : $@convention(c) (@inout LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper
// CHECK: apply [[OP]]([[SELFACCESS]], [[RHS]]) : $@convention(c) (@inout LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper
// CHECK: end_access [[SELFACCESS]] : $*LoadableIntWrapper

// CHECK: sil [clang LoadableIntWrapper."-"] [[NAME]] : $@convention(c) (@inout LoadableIntWrapper, LoadableIntWrapper) -> LoadableIntWrapper

public func call(_ wrapper: inout LoadableIntWrapper, _ arg: Int32) -> Int32 { wrapper(arg) }

// CHECK: bb0([[SELF:%.*]] : $*LoadableIntWrapper, [[RHS:%.*]] : $Int32):
// CHECK: [[SELFACCESS:%.*]] = begin_access [modify] [static] [[SELF]] : $*LoadableIntWrapper
// CHECK: [[OP:%.*]] = function_ref [[NAME:@(_ZN18LoadableIntWrapperclEi|\?\?RLoadableIntWrapper@@QEAAHH@Z)]] : $@convention(c) (@inout LoadableIntWrapper, Int32) -> Int32
// CHECK: apply [[OP]]([[SELFACCESS]], [[RHS]]) : $@convention(c) (@inout LoadableIntWrapper, Int32) -> Int32
// CHECK: end_access [[SELFACCESS]] : $*LoadableIntWrapper

// CHECK: sil [clang LoadableIntWrapper.callAsFunction] [[NAME]] : $@convention(c) (@inout LoadableIntWrapper, Int32) -> Int32

public func call(_ wrapper: inout AddressOnlyIntWrapper) -> Int32 { wrapper() }

// CHECK: bb0([[SELF:%.*]] : $*AddressOnlyIntWrapper):
// CHECK: [[SELFACCESS:%.*]] = begin_access [modify] [static] [[SELF]] : $*AddressOnlyIntWrapper
// CHECK: [[OP:%.*]] = function_ref [[NAME:@(_ZN21AddressOnlyIntWrapperclEv|\?\?RAddressOnlyIntWrapper@@QEAAHXZ)]] : $@convention(c) (@inout AddressOnlyIntWrapper) -> Int32
// CHECK: apply [[OP]]([[SELFACCESS]]) : $@convention(c) (@inout AddressOnlyIntWrapper) -> Int32
// CHECK: end_access [[SELFACCESS]] : $*AddressOnlyIntWrapper

// CHECK: sil [clang AddressOnlyIntWrapper.callAsFunction] [[NAME]] : $@convention(c) (@inout AddressOnlyIntWrapper) -> Int32

public func index(_ arr: inout ReadOnlyIntArray, _ arg: Int32) -> Int32 { arr[arg] }

// CHECK: sil @$s4main5indexys5Int32VSo16ReadOnlyIntArrayVz_ADtF : $@convention(thin) (@inout ReadOnlyIntArray, Int32) -> Int32 {
// CHECK: bb0([[ARR:%.*]] : $*ReadOnlyIntArray, [[INDEX:%.*]] : $Int32):
// CHECK:   [[ARRACCESS:%.*]] = begin_access [modify] [static] [[ARR]] : $*ReadOnlyIntArray
// CHECK:   [[ARRACCESS2:%.*]] = begin_access [modify] [static] [[ARRACCESS]] : $*ReadOnlyIntArray
// CHECK:   [[OP:%.*]] = function_ref [[READCLASSNAME:@(_ZNK16ReadOnlyIntArrayixEi|\?\?AReadOnlyIntArray@@QEBAAEBHH@Z)]] : $@convention(c) (@inout ReadOnlyIntArray, Int32) -> UnsafePointer<Int32>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[ARRACCESS2]], [[INDEX]]) : $@convention(c) (@inout ReadOnlyIntArray, Int32) -> UnsafePointer<Int32>
// CHECK: } // end sil function '$s4main5indexys5Int32VSo16ReadOnlyIntArrayVz_ADtF'

// CHECK: sil shared [transparent] [serializable] @$sSo16ReadOnlyIntArrayVys5Int32VADcig : $@convention(method) (Int32, @inout ReadOnlyIntArray) -> Int32 {
// CHECK: bb0([[INDEX:%.*]] : $Int32, [[SELF:%.*]] : $*ReadOnlyIntArray):
// CHECK:   [[SELFACCESS:%.*]] = begin_access [modify] [static] [[SELF]] : $*ReadOnlyIntArray
// CHECK:   [[OP:%.*]] = function_ref [[READCLASSNAME]] : $@convention(c) (@inout ReadOnlyIntArray, Int32) -> UnsafePointer<Int32>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[SELFACCESS]], [[INDEX]]) : $@convention(c) (@inout ReadOnlyIntArray, Int32) -> UnsafePointer<Int32>
// CHECK:   end_access [[SELFACCESS]] : $*ReadOnlyIntArray
// CHECK:   [[PTR2:%.*]] = struct_extract [[PTR]] : $UnsafePointer<Int32>, #UnsafePointer._rawValue
// CHECK:   pointer_to_address [[PTR2]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK: } // end sil function '$sSo16ReadOnlyIntArrayVys5Int32VADcig'

public func index(_ arr: inout ReadWriteIntArray, _ arg: Int32, _ val: Int32) { arr[arg] = val }

// CHECK: sil @$s4main5indexyySo17ReadWriteIntArrayVz_s5Int32VAFtF : $@convention(thin) (@inout ReadWriteIntArray, Int32, Int32) -> () {
// CHECK: bb0([[ARR:%.*]] : $*ReadWriteIntArray, [[INDEX:%.*]] : $Int32, [[NEWVALUE:%.*]] : $Int32):
// CHECK:   [[ARRACCESS:%.*]] = begin_access [modify] [static] [[ARR]] : $*ReadWriteIntArray
// CHECK:   [[ARRACCESS2:%.*]] = begin_access [modify] [static] [[ARRACCESS]] : $*ReadWriteIntArray
// CHECK:   [[OP:%.*]] = function_ref [[READWRITECLASSNAME:@(_ZN17ReadWriteIntArrayixEi|\?\?AReadWriteIntArray@@QEAAAEAHH@Z)]] : $@convention(c) (@inout ReadWriteIntArray, Int32) -> UnsafeMutablePointer<Int32>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[ARRACCESS2]], [[INDEX]]) : $@convention(c) (@inout ReadWriteIntArray, Int32) -> UnsafeMutablePointer<Int32>
// CHECK: } // end sil function '$s4main5indexyySo17ReadWriteIntArrayVz_s5Int32VAFtF'

// CHECK: sil shared [transparent] [serializable] @$sSo17ReadWriteIntArrayVys5Int32VADcis : $@convention(method) (Int32, Int32, @inout ReadWriteIntArray) -> () {
// CHECK: bb0([[NEWVALUE:%.*]] : $Int32, [[INDEX:%.*]] : $Int32, [[SELF:%.*]] : $*ReadWriteIntArray):
// CHECK:   [[SELFACCESS:%.*]] = begin_access [modify] [static] [[SELF]] : $*ReadWriteIntArray
// CHECK:   [[OP:%.*]] = function_ref [[READWRITECLASSNAME]] : $@convention(c) (@inout ReadWriteIntArray, Int32) -> UnsafeMutablePointer<Int32>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[SELFACCESS]], [[INDEX]]) : $@convention(c) (@inout ReadWriteIntArray, Int32) -> UnsafeMutablePointer<Int32>
// CHECK:   end_access [[SELFACCESS]] : $*ReadWriteIntArray
// CHECK:   [[PTR2:%.*]] = struct_extract [[PTR]] : $UnsafeMutablePointer<Int32>, #UnsafeMutablePointer._rawValue
// CHECK:   pointer_to_address [[PTR2]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK: } // end sil function '$sSo17ReadWriteIntArrayVys5Int32VADcis'

// CHECK: sil [clang ReadOnlyIntArray.__operatorSubscriptConst] [[READCLASSNAME]] : $@convention(c) (@inout ReadOnlyIntArray, Int32) -> UnsafePointer<Int32>
// CHECK: sil [clang ReadWriteIntArray.__operatorSubscript] [[READWRITECLASSNAME]] : $@convention(c) (@inout ReadWriteIntArray, Int32) -> UnsafeMutablePointer<Int32>
