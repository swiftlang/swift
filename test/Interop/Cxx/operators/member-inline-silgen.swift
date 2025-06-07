// RUN: %target-swift-emit-sil -Xllvm -sil-print-types %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s

import MemberInline

public func sub(_ lhs: inout LoadableIntWrapper, _ rhs: LoadableIntWrapper) -> LoadableIntWrapper { lhs - rhs }

// CHECK: bb0([[SELF:%.*]] : $*LoadableIntWrapper, [[RHS:%.*]] : $LoadableIntWrapper):
// CHECK: [[METATYPE:%.*]] = metatype $@thin LoadableIntWrapper.Type
// CHECK: [[SELFACCESS:%.*]] = begin_access [modify] [static] [[SELF]] : $*LoadableIntWrapper
// CHECK: [[OP:%.*]] = function_ref @$sSo18LoadableIntWrapperV1soiyA2Bz_ABtFZ : $@convention(method) (@inout LoadableIntWrapper, LoadableIntWrapper, @thin LoadableIntWrapper.Type) -> LoadableIntWrapper
// CHECK: apply [[OP]]([[SELFACCESS]], [[RHS]], [[METATYPE]]) : $@convention(method) (@inout LoadableIntWrapper, LoadableIntWrapper, @thin LoadableIntWrapper.Type) -> LoadableIntWrapper
// CHECK: end_access [[SELFACCESS]] : $*LoadableIntWrapper

public func exclaim(_ wrapper: inout LoadableBoolWrapper) -> LoadableBoolWrapper { !wrapper }

// CHECK: bb0([[SELF:%.*]] : $*LoadableBoolWrapper):
// CHECK:   [[METATYPE:%.*]] = metatype $@thin LoadableBoolWrapper.Type
// CHECK:   [[SELFACCESS:%.*]] = begin_access [modify] [static] [[SELF]] : $*LoadableBoolWrapper
// CHECK:   [[OP:%.*]] = function_ref @$sSo19LoadableBoolWrapperV1nopyA2BzFZ : $@convention(method) (@inout LoadableBoolWrapper, @thin LoadableBoolWrapper.Type) -> LoadableBoolWrapper
// CHECK:   apply [[OP]]([[SELFACCESS]], [[METATYPE]]) : $@convention(method) (@inout LoadableBoolWrapper, @thin LoadableBoolWrapper.Type) -> LoadableBoolWrapper
// CHECK:   end_access [[SELFACCESS]]

public func call(_ wrapper: inout LoadableIntWrapper, _ arg: Int32) -> Int32 { wrapper(arg) }

// CHECK: bb0([[SELF:%.*]] : $*LoadableIntWrapper, [[RHS:%.*]] : $Int32):
// CHECK: [[SELFACCESS:%.*]] = begin_access [modify] [static] [[SELF]] : $*LoadableIntWrapper
// CHECK: [[OP:%.*]] = function_ref [[NAME:@(_ZN18LoadableIntWrapperclEi|\?\?RLoadableIntWrapper@@QEAAHH@Z)]] : $@convention(cxx_method) (Int32, @inout LoadableIntWrapper) -> Int32
// CHECK: apply [[OP]]([[RHS]], [[SELFACCESS]]) : $@convention(cxx_method) (Int32, @inout LoadableIntWrapper) -> Int32
// CHECK: end_access [[SELFACCESS]] : $*LoadableIntWrapper

// CHECK: sil [clang LoadableIntWrapper.callAsFunction] [[NAME]] : $@convention(cxx_method) (Int32, @inout LoadableIntWrapper) -> Int32

public func call(_ wrapper: inout AddressOnlyIntWrapper) -> Int32 { wrapper() }

// CHECK: bb0([[SELF:%.*]] : $*AddressOnlyIntWrapper):
// CHECK: [[SELFACCESS:%.*]] = begin_access [modify] [static] [[SELF]] : $*AddressOnlyIntWrapper
// CHECK: [[OP:%.*]] = function_ref [[NAME:@(_ZN21AddressOnlyIntWrapperclEv|\?\?RAddressOnlyIntWrapper@@QEAAHXZ)]] : $@convention(cxx_method) (@inout AddressOnlyIntWrapper) -> Int32
// CHECK: apply [[OP]]([[SELFACCESS]]) : $@convention(cxx_method) (@inout AddressOnlyIntWrapper) -> Int32
// CHECK: end_access [[SELFACCESS]] : $*AddressOnlyIntWrapper

// CHECK: sil [clang AddressOnlyIntWrapper.callAsFunction] [[NAME]] : $@convention(cxx_method) (@inout AddressOnlyIntWrapper) -> Int32

public func index(_ arr: ReadOnlyIntArray, _ arg: Int32) -> Int32 { arr[arg] }

// CHECK: sil @$s4main5indexys5Int32VSo16ReadOnlyIntArrayV_ADtF : $@convention(thin) (@in_guaranteed ReadOnlyIntArray, Int32) -> Int32 {
// CHECK: bb0([[ARR:%.*]] : $*ReadOnlyIntArray, [[INDEX:%.*]] : $Int32):
// CHECK:   [[OP:%.*]] = function_ref [[READCLASSNAME:@(_ZNK16ReadOnlyIntArrayixEi|\?\?AReadOnlyIntArray@@QEBAAEBHH@Z)]] : $@convention(cxx_method) (Int32, @in_guaranteed ReadOnlyIntArray) -> UnsafePointer<Int32>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[INDEX]], [[ARRACCESS:%.*]]) : $@convention(cxx_method) (Int32, @in_guaranteed ReadOnlyIntArray) -> UnsafePointer<Int32>
// CHECK: } // end sil function '$s4main5indexys5Int32VSo16ReadOnlyIntArrayV_ADtF'

// CHECK: sil shared [transparent] @$sSo16ReadOnlyIntArrayVys5Int32VADcig : $@convention(method) (Int32, @in_guaranteed ReadOnlyIntArray) -> Int32 {
// CHECK: bb0([[INDEX:%.*]] : $Int32, [[SELF:%.*]] : $*ReadOnlyIntArray):
// CHECK:   [[OP:%.*]] = function_ref [[READCLASSNAME]] : $@convention(cxx_method) (Int32, @in_guaranteed ReadOnlyIntArray) -> UnsafePointer<Int32>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[INDEX]], [[SELFACCESS:%.*]]) : $@convention(cxx_method) (Int32, @in_guaranteed ReadOnlyIntArray) -> UnsafePointer<Int32>
// CHECK:   [[PTR2:%.*]] = struct_extract [[PTR]] : $UnsafePointer<Int32>, #UnsafePointer._rawValue
// CHECK:   pointer_to_address [[PTR2]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK: } // end sil function '$sSo16ReadOnlyIntArrayVys5Int32VADcig'

public func index(_ arr: inout ReadWriteIntArray, _ arg: Int32, _ val: Int32) { arr[arg] = val }

// CHECK: sil @$s4main5indexyySo17ReadWriteIntArrayVz_s5Int32VAFtF : $@convention(thin) (@inout ReadWriteIntArray, Int32, Int32) -> () {
// CHECK: bb0([[ARR:%.*]] : $*ReadWriteIntArray, [[INDEX:%.*]] : $Int32, [[NEWVALUE:%.*]] : $Int32):
// CHECK:   [[ARRACCESS:%.*]] = begin_access [modify] [static] [[ARR]] : $*ReadWriteIntArray
// CHECK:   [[ARRACCESS2:%.*]] = begin_access [modify] [static] [[ARRACCESS]] : $*ReadWriteIntArray
// CHECK:   [[OP:%.*]] = function_ref [[READWRITECLASSNAME:@(_ZN17ReadWriteIntArrayixEi|\?\?AReadWriteIntArray@@QEAAAEAHH@Z)]] : $@convention(cxx_method) (Int32, @inout ReadWriteIntArray) -> UnsafeMutablePointer<Int32>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[INDEX]], [[ARRACCESS2]]) : $@convention(cxx_method) (Int32, @inout ReadWriteIntArray) -> UnsafeMutablePointer<Int32>
// CHECK: } // end sil function '$s4main5indexyySo17ReadWriteIntArrayVz_s5Int32VAFtF'

// CHECK: sil shared [transparent] @$sSo17ReadWriteIntArrayVys5Int32VADcis : $@convention(method) (Int32, Int32, @inout ReadWriteIntArray) -> () {
// CHECK: bb0([[NEWVALUE:%.*]] : $Int32, [[INDEX:%.*]] : $Int32, [[SELF:%.*]] : $*ReadWriteIntArray):
// CHECK:   [[SELFACCESS:%.*]] = begin_access [modify] [static] [[SELF]] : $*ReadWriteIntArray
// CHECK:   [[OP:%.*]] = function_ref [[READWRITECLASSNAME]] : $@convention(cxx_method) (Int32, @inout ReadWriteIntArray) -> UnsafeMutablePointer<Int32>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[INDEX]], [[SELFACCESS]]) : $@convention(cxx_method) (Int32, @inout ReadWriteIntArray) -> UnsafeMutablePointer<Int32>
// CHECK:   end_access [[SELFACCESS]] : $*ReadWriteIntArray
// CHECK:   [[PTR2:%.*]] = struct_extract [[PTR]] : $UnsafeMutablePointer<Int32>, #UnsafeMutablePointer._rawValue
// CHECK:   pointer_to_address [[PTR2]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK: } // end sil function '$sSo17ReadWriteIntArrayVys5Int32VADcis'

public func index(_ arr: inout NonTrivialIntArrayByVal, _ arg: Int32, _ val: Int32) -> Int32 { arr[arg] }

// CHECK: sil @$s4main5indexys5Int32VSo23NonTrivialIntArrayByValVz_A2DtF : $@convention(thin) (@inout NonTrivialIntArrayByVal, Int32, Int32) -> Int32 {
// CHECK: bb0([[ARR:%.*]] : $*NonTrivialIntArrayByVal, [[INDEX:%.*]] : $Int32, [[NEWVALUE:%.*]] : $Int32):
// CHECK:   [[OP:%.*]] = function_ref [[READWRITECLASSNAMEBYVAL:@(_ZNK23NonTrivialIntArrayByValixEi|\?\?ANonTrivialIntArrayByVal@@QEBAHH@Z)]] : $@convention(cxx_method) (Int32, @in_guaranteed NonTrivialIntArrayByVal) -> Int32
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[INDEX]], [[ARRACCESS:%.*]]) : $@convention(cxx_method) (Int32, @in_guaranteed NonTrivialIntArrayByVal) -> Int32
// CHECK: } // end sil function '$s4main5indexys5Int32VSo23NonTrivialIntArrayByValVz_A2DtF'

// CHECK: sil shared [transparent] @$sSo23NonTrivialIntArrayByValVys5Int32VADcig : $@convention(method) (Int32, @in_guaranteed NonTrivialIntArrayByVal) -> Int32 {
// CHECK: bb0([[NEWVALUE:%.*]] : $Int32, [[INDEX:%.*]] : $*NonTrivialIntArrayByVal):
// CHECK:   [[OP:%.*]] = function_ref [[READWRITECLASSNAMEBYVAL]] : $@convention(cxx_method) (Int32, @in_guaranteed NonTrivialIntArrayByVal) -> Int32
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[NEWVALUE]], [[SELFACCESS:%.*]]) : $@convention(cxx_method) (Int32, @in_guaranteed NonTrivialIntArrayByVal) -> Int32
// CHECK: } // end sil function '$sSo23NonTrivialIntArrayByValVys5Int32VADcig

public func index(_ arr: inout PtrByVal, _ arg: Int32, _ val: Int32) -> Int32 { arr[arg]![0] }
// CHECK: sil @$s4main5indexys5Int32VSo8PtrByValVz_A2DtF : $@convention(thin) (@inout PtrByVal, Int32, Int32) -> Int32 {
// CHECK: bb0([[ARR:%.*]] : $*PtrByVal, [[INDEX:%.*]] : $Int32, [[NEWVALUE:%.*]] : $Int32):
// CHECK:   [[ARRACCESS:%.*]] = begin_access [modify] [static] [[ARR]] : $*PtrByVal
// CHECK:   [[ARRACCESS2:%.*]] = begin_access [modify] [static] [[ARRACCESS]] : $*PtrByVal
// CHECK:   [[OP:%.*]] = function_ref [[PTRBYVAL:@(_ZN8PtrByValixEi|\?\?APtrByVal@@QEAAPEAHH@Z)]] : $@convention(cxx_method) (Int32, @inout PtrByVal) -> Optional<UnsafeMutablePointer<Int32>>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[INDEX]], [[ARRACCESS2]]) : $@convention(cxx_method) (Int32, @inout PtrByVal) -> Optional<UnsafeMutablePointer<Int32>>
// CHECK: } // end sil function '$s4main5indexys5Int32VSo8PtrByValVz_A2DtF'

// CHECK: sil shared [transparent] @$sSo8PtrByValVySpys5Int32VGSgADcig : $@convention(method) (Int32, @inout PtrByVal) -> Optional<UnsafeMutablePointer<Int32>> {
// CHECK: bb0([[NEWVALUE:%.*]] : $Int32, [[INDEX:%.*]] : $*PtrByVal):
// CHECK:   [[SELFACCESS:%.*]] = begin_access [modify] [static] [[INDEX]] : $*PtrByVal
// CHECK:   [[OP:%.*]] = function_ref [[PTRBYVAL]] : $@convention(cxx_method) (Int32, @inout PtrByVal) -> Optional<UnsafeMutablePointer<Int32>>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[NEWVALUE]], [[SELFACCESS]]) : $@convention(cxx_method) (Int32, @inout PtrByVal) -> Optional<UnsafeMutablePointer<Int32>>
// CHECK:   end_access [[SELFACCESS]] : $*PtrByVal
// CHECK: } // end sil function '$sSo8PtrByValVySpys5Int32VGSgADcig

public func index(_ arr: inout RefToPtr, _ arg: Int32, _ val: Int32) -> Int32 { arr[arg]![0] }
// CHECK: sil @$s4main5indexys5Int32VSo8RefToPtrVz_A2DtF : $@convention(thin) (@inout RefToPtr, Int32, Int32) -> Int32 {
// CHECK: bb0([[ARR:%.*]] : $*RefToPtr, [[INDEX:%.*]] : $Int32, [[NEWVALUE:%.*]] : $Int32):
// CHECK:   [[ARRACCESS:%.*]] = begin_access [modify] [static] [[ARR]] : $*RefToPtr
// CHECK:   [[ARRACCESS2:%.*]] = begin_access [modify] [static] [[ARRACCESS]] : $*RefToPtr
// CHECK:   [[OP:%.*]] = function_ref [[REFTOPTR:@(_ZN8RefToPtrixEi|\?\?ARefToPtr@@QEAAAEAPEAHH@Z)]] : $@convention(cxx_method) (Int32, @inout RefToPtr) -> UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int32>>>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[INDEX]], [[ARRACCESS2]]) : $@convention(cxx_method) (Int32, @inout RefToPtr) -> UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int32>>>
// CHECK: } // end sil function '$s4main5indexys5Int32VSo8RefToPtrVz_A2DtF'

// CHECK: sil shared [transparent] @$sSo8RefToPtrVySpys5Int32VGSgADcig : $@convention(method) (Int32, @inout RefToPtr) -> Optional<UnsafeMutablePointer<Int32>> {
// CHECK: bb0([[NEWVALUE:%.*]] : $Int32, [[INDEX:%.*]] : $*RefToPtr):
// CHECK:   [[SELFACCESS:%.*]] = begin_access [modify] [static] [[INDEX]] : $*RefToPtr
// CHECK:   [[OP:%.*]] = function_ref [[REFTOPTR]] : $@convention(cxx_method) (Int32, @inout RefToPtr) -> UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int32>>>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[NEWVALUE]], [[SELFACCESS]]) : $@convention(cxx_method) (Int32, @inout RefToPtr) -> UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int32>>>
// CHECK:   end_access [[SELFACCESS]] : $*RefToPtr
// CHECK: } // end sil function '$sSo8RefToPtrVySpys5Int32VGSgADcig

public func index(_ arr: inout PtrToPtr, _ arg: Int32, _ val: Int32) -> Int32 { arr[arg]![0]![0] }
// CHECK: sil @$s4main5indexys5Int32VSo05PtrToD0Vz_A2DtF : $@convention(thin) (@inout PtrToPtr, Int32, Int32) -> Int32 {
// CHECK: bb0([[ARR:%.*]] : $*PtrToPtr, [[INDEX:%.*]] : $Int32, [[NEWVALUE:%.*]] : $Int32):
// CHECK:   [[ARRACCESS:%.*]] = begin_access [modify] [static] [[ARR]] : $*PtrToPtr
// CHECK:   [[ARRACCESS2:%.*]] = begin_access [modify] [static] [[ARRACCESS]] : $*PtrToPtr
// CHECK:   [[OP:%.*]] = function_ref [[PTRTOPTR:@(_ZN8PtrToPtrixEi|\?\?APtrToPtr@@QEAAPEAPEAHH@Z)]] : $@convention(cxx_method) (Int32, @inout PtrToPtr) -> Optional<UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int32>>>>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[INDEX]], [[ARRACCESS2]]) : $@convention(cxx_method) (Int32, @inout PtrToPtr) -> Optional<UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int32>>>>
// CHECK: } // end sil function '$s4main5indexys5Int32VSo05PtrToD0Vz_A2DtF'

// CHECK: sil shared [transparent] @$sSo05PtrToA0VySpySpys5Int32VGSgGSgADcig : $@convention(method) (Int32, @inout PtrToPtr) -> Optional<UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int32>>>> {
// CHECK: bb0([[NEWVALUE:%.*]] : $Int32, [[INDEX:%.*]] : $*PtrToPtr):
// CHECK:   [[SELFACCESS:%.*]] = begin_access [modify] [static] [[INDEX]] : $*PtrToPtr
// CHECK:   [[OP:%.*]] = function_ref [[PTRTOPTR]] : $@convention(cxx_method) (Int32, @inout PtrToPtr) -> Optional<UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int32>>>>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[NEWVALUE]], [[SELFACCESS]]) : $@convention(cxx_method) (Int32, @inout PtrToPtr) -> Optional<UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int32>>>>
// CHECK:   end_access [[SELFACCESS]] : $*PtrToPtr
// CHECK: } // end sil function '$sSo05PtrToA0VySpySpys5Int32VGSgGSgADcig

public func index(_ arr: ConstOpPtrByVal, _ arg: Int32, _ val: Int32) -> Int32 { arr[arg]![0] }
// CHECK: sil @$s4main5indexys5Int32VSo15ConstOpPtrByValV_A2DtF : $@convention(thin) (ConstOpPtrByVal, Int32, Int32) -> Int32 {
// CHECK: bb0([[ARR:%.*]] : $ConstOpPtrByVal, [[INDEX:%.*]] : $Int32, [[NEWVALUE:%.*]] : $Int32):
// CHECK:   [[OP:%.*]] = function_ref [[CONSTOPPTRBYVAL:@(_ZNK15ConstOpPtrByValixEi|\?\?AConstOpPtrByVal@@QEBAPEBHH@Z)]] : $@convention(cxx_method) (Int32, @in_guaranteed ConstOpPtrByVal) -> Optional<UnsafePointer<Int32>>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[INDEX]], [[ARRACCESS2:%.*]]) : $@convention(cxx_method) (Int32, @in_guaranteed ConstOpPtrByVal) -> Optional<UnsafePointer<Int32>>
// CHECK: } // end sil function '$s4main5indexys5Int32VSo15ConstOpPtrByValV_A2DtF'

// CHECK: sil shared [transparent] @$sSo15ConstOpPtrByValVySPys5Int32VGSgADcig : $@convention(method) (Int32, ConstOpPtrByVal) -> Optional<UnsafePointer<Int32>> {
// CHECK: bb0([[NEWVALUE:%.*]] : $Int32, [[INDEX:%.*]] : $ConstOpPtrByVal):
// CHECK:   [[OP:%.*]] = function_ref [[CONSTOPPTRBYVAL]] : $@convention(cxx_method) (Int32, @in_guaranteed ConstOpPtrByVal) -> Optional<UnsafePointer<Int32>>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[NEWVALUE]], [[SELFACCESS:%.*]]) : $@convention(cxx_method) (Int32, @in_guaranteed ConstOpPtrByVal) -> Optional<UnsafePointer<Int32>>
// CHECK: } // end sil function '$sSo15ConstOpPtrByValVySPys5Int32VGSgADcig

public func index(_ arr: inout ConstPtrByVal, _ arg: Int32, _ val: Int32) -> Int32 { arr[arg]![0] }
// CHECK: sil @$s4main5indexys5Int32VSo13ConstPtrByValVz_A2DtF : $@convention(thin) (@inout ConstPtrByVal, Int32, Int32) -> Int32 {
// CHECK: bb0([[ARR:%.*]] : $*ConstPtrByVal, [[INDEX:%.*]] : $Int32, [[NEWVALUE:%.*]] : $Int32):
// CHECK:   [[ARRACCESS:%.*]] = begin_access [modify] [static] [[ARR]] : $*ConstPtrByVal
// CHECK:   [[ARRACCESS2:%.*]] = begin_access [modify] [static] [[ARRACCESS]] : $*ConstPtrByVal
// CHECK:   [[OP:%.*]] = function_ref [[CONSTPTRBYVAL:@(_ZN13ConstPtrByValixEi|\?\?AConstPtrByVal@@QEAAPEBHH@Z)]] : $@convention(cxx_method) (Int32, @inout ConstPtrByVal) -> Optional<UnsafePointer<Int32>>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[INDEX]], [[ARRACCESS2]]) : $@convention(cxx_method) (Int32, @inout ConstPtrByVal) -> Optional<UnsafePointer<Int32>>
// CHECK: } // end sil function '$s4main5indexys5Int32VSo13ConstPtrByValVz_A2DtF'

// CHECK: sil shared [transparent] @$sSo13ConstPtrByValVySPys5Int32VGSgADcig : $@convention(method) (Int32, @inout ConstPtrByVal) -> Optional<UnsafePointer<Int32>> {
// CHECK: bb0([[NEWVALUE:%.*]] : $Int32, [[INDEX:%.*]] : $*ConstPtrByVal):
// CHECK:   [[SELFACCESS:%.*]] = begin_access [modify] [static] [[INDEX]] : $*ConstPtrByVal
// CHECK:   [[OP:%.*]] = function_ref [[CONSTPTRBYVAL]] : $@convention(cxx_method) (Int32, @inout ConstPtrByVal) -> Optional<UnsafePointer<Int32>>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[NEWVALUE]], [[SELFACCESS]]) : $@convention(cxx_method) (Int32, @inout ConstPtrByVal) -> Optional<UnsafePointer<Int32>>
// CHECK:   end_access [[SELFACCESS]] : $*ConstPtrByVal
// CHECK: } // end sil function '$sSo13ConstPtrByValVySPys5Int32VGSgADcig

public func subscriptUnnamed(_ unnamed: SubscriptUnnamedParameter, _ arg: Int32) -> Int32 { unnamed[arg] }
// CHECK: sil shared [transparent] @$sSo25SubscriptUnnamedParameterVys5Int32VADcig : $@convention(method) (Int32, SubscriptUnnamedParameter) -> Int32 {
// CHECK: bb0([[INDEX:%.*]] : $Int32, [[SELF:%.*]] : $SubscriptUnnamedParameter):
// CHECK:   [[SELFACCESS:%.*]] = alloc_stack $SubscriptUnnamedParameter
// CHECK:   [[OP:%.*]] = function_ref [[OPERATORNAME:@(_ZNK25SubscriptUnnamedParameterixEi|\?\?ASubscriptUnnamedParameter@@QEBAHH@Z)]] : $@convention(cxx_method) (Int32, @in_guaranteed SubscriptUnnamedParameter) -> Int32
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[INDEX]], [[SELFACCESS]]) : $@convention(cxx_method) (Int32, @in_guaranteed SubscriptUnnamedParameter) -> Int32
// CHECK:   dealloc_stack [[SELFACCESS]]
// CHECK: } // end sil function '$sSo25SubscriptUnnamedParameterVys5Int32VADcig'

// CHECK: sil [clang ReadOnlyIntArray.__operatorSubscriptConst] [[READCLASSNAME]] : $@convention(cxx_method) (Int32, @in_guaranteed ReadOnlyIntArray) -> UnsafePointer<Int32>
// CHECK: sil [clang ReadWriteIntArray.__operatorSubscript] [[READWRITECLASSNAME]] : $@convention(cxx_method) (Int32, @inout ReadWriteIntArray) -> UnsafeMutablePointer<Int32>
// CHECK: sil [clang NonTrivialIntArrayByVal.__operatorSubscriptConst] [[READWRITECLASSNAMEBYVAL]] : $@convention(cxx_method) (Int32, @in_guaranteed NonTrivialIntArrayByVal) -> Int32

// CHECK: sil [clang PtrByVal.__operatorSubscript] [[PTRBYVAL]] : $@convention(cxx_method) (Int32, @inout PtrByVal) -> Optional<UnsafeMutablePointer<Int32>>
// CHECK: sil [clang RefToPtr.__operatorSubscript] [[REFTOPTR]] : $@convention(cxx_method) (Int32, @inout RefToPtr) -> UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int32>>>
// CHECK: sil [clang PtrToPtr.__operatorSubscript] [[PTRTOPTR]] : $@convention(cxx_method) (Int32, @inout PtrToPtr) -> Optional<UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int32>>>>
// CHECK: sil [clang ConstOpPtrByVal.__operatorSubscriptConst] [[CONSTOPPTRBYVAL]] : $@convention(cxx_method) (Int32, @in_guaranteed ConstOpPtrByVal) -> Optional<UnsafePointer<Int32>>
// CHECK: sil [clang ConstPtrByVal.__operatorSubscriptConst] [[CONSTPTRBYVAL]] : $@convention(cxx_method) (Int32, @inout ConstPtrByVal) -> Optional<UnsafePointer<Int32>>
