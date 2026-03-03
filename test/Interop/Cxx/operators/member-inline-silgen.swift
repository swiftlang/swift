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
// CHECK: [[OP:%.*]] = function_ref @$sSo18LoadableIntWrapperV14callAsFunctionys5Int32VAEFTo : $@convention(cxx_method) (Int32, @inout LoadableIntWrapper) -> Int32
// CHECK: apply [[OP]]([[RHS]], [[SELFACCESS]]) : $@convention(cxx_method) (Int32, @inout LoadableIntWrapper) -> Int32
// CHECK: end_access [[SELFACCESS]] : $*LoadableIntWrapper

// CHECK: sil [asmname "{{.*}}"] [clang LoadableIntWrapper.callAsFunction] @$sSo18LoadableIntWrapperV14callAsFunctionys5Int32VAEFTo : $@convention(cxx_method) (Int32, @inout LoadableIntWrapper) -> Int32

public func call(_ wrapper: inout AddressOnlyIntWrapper) -> Int32 { wrapper() }

// CHECK: bb0([[SELF:%.*]] : $*AddressOnlyIntWrapper):
// CHECK: [[SELFACCESS:%.*]] = begin_access [modify] [static] [[SELF]] : $*AddressOnlyIntWrapper
// CHECK: [[OP:%.*]] = function_ref @$sSo21AddressOnlyIntWrapperV14callAsFunctions5Int32VyFTo : $@convention(cxx_method) (@inout AddressOnlyIntWrapper) -> Int32
// CHECK: apply [[OP]]([[SELFACCESS]]) : $@convention(cxx_method) (@inout AddressOnlyIntWrapper) -> Int32
// CHECK: end_access [[SELFACCESS]] : $*AddressOnlyIntWrapper

// CHECK: sil [asmname "{{.*}}"] [clang AddressOnlyIntWrapper.callAsFunction] @$sSo21AddressOnlyIntWrapperV14callAsFunctions5Int32VyFTo : $@convention(cxx_method) (@inout AddressOnlyIntWrapper) -> Int32

public func index(_ arr: ReadOnlyIntArray, _ arg: Int32) -> Int32 { arr[arg] }

// CHECK: sil @$s4main5indexys5Int32VSo16ReadOnlyIntArrayV_ADtF : $@convention(thin) (@in_guaranteed ReadOnlyIntArray, Int32) -> Int32 {
// CHECK: bb0([[ARR:%.*]] : $*ReadOnlyIntArray, [[INDEX:%.*]] : $Int32):
// CHECK:   [[OP:%.*]] = function_ref @$sSo16ReadOnlyIntArrayV24__operatorSubscriptConstySPys5Int32VGAEFTo : $@convention(cxx_method) (Int32, @in_guaranteed ReadOnlyIntArray) -> UnsafePointer<Int32>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[INDEX]], [[ARRACCESS:%.*]]) : $@convention(cxx_method) (Int32, @in_guaranteed ReadOnlyIntArray) -> UnsafePointer<Int32>
// CHECK: } // end sil function '$s4main5indexys5Int32VSo16ReadOnlyIntArrayV_ADtF'

// CHECK: sil shared [transparent] @$sSo16ReadOnlyIntArrayVys5Int32VADcig : $@convention(method) (Int32, @in_guaranteed ReadOnlyIntArray) -> Int32 {
// CHECK: bb0([[INDEX:%.*]] : $Int32, [[SELF:%.*]] : $*ReadOnlyIntArray):
// CHECK:   [[OP:%.*]] = function_ref @$sSo16ReadOnlyIntArrayV24__operatorSubscriptConstySPys5Int32VGAEFTo : $@convention(cxx_method) (Int32, @in_guaranteed ReadOnlyIntArray) -> UnsafePointer<Int32>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[INDEX]], [[SELFACCESS:%.*]]) : $@convention(cxx_method) (Int32, @in_guaranteed ReadOnlyIntArray) -> UnsafePointer<Int32>
// CHECK:   [[PTR2:%.*]] = struct_extract [[PTR]] : $UnsafePointer<Int32>, #UnsafePointer._rawValue
// CHECK:   pointer_to_address [[PTR2]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK: } // end sil function '$sSo16ReadOnlyIntArrayVys5Int32VADcig'

public func index(_ arr: inout ReadWriteIntArray, _ arg: Int32, _ val: Int32) { arr[arg] = val }

// CHECK: sil @$s4main5indexyySo17ReadWriteIntArrayVz_s5Int32VAFtF : $@convention(thin) (@inout ReadWriteIntArray, Int32, Int32) -> () {
// CHECK: bb0([[ARR:%.*]] : $*ReadWriteIntArray, [[INDEX:%.*]] : $Int32, [[NEWVALUE:%.*]] : $Int32):
// CHECK:   [[ARRACCESS:%.*]] = begin_access [modify] [static] [[ARR]] : $*ReadWriteIntArray
// CHECK:   [[ARRACCESS2:%.*]] = begin_access [modify] [static] [[ARRACCESS]] : $*ReadWriteIntArray
// CHECK:   [[OP:%.*]] = function_ref @$sSo17ReadWriteIntArrayV19__operatorSubscriptySpys5Int32VGAEFTo : $@convention(cxx_method) (Int32, @inout ReadWriteIntArray) -> UnsafeMutablePointer<Int32>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[INDEX]], [[ARRACCESS2]]) : $@convention(cxx_method) (Int32, @inout ReadWriteIntArray) -> UnsafeMutablePointer<Int32>
// CHECK: } // end sil function '$s4main5indexyySo17ReadWriteIntArrayVz_s5Int32VAFtF'

// CHECK: sil shared [transparent] @$sSo17ReadWriteIntArrayVys5Int32VADcis : $@convention(method) (Int32, Int32, @inout ReadWriteIntArray) -> () {
// CHECK: bb0([[NEWVALUE:%.*]] : $Int32, [[INDEX:%.*]] : $Int32, [[SELF:%.*]] : $*ReadWriteIntArray):
// CHECK:   [[SELFACCESS:%.*]] = begin_access [modify] [static] [[SELF]] : $*ReadWriteIntArray
// CHECK:   [[OP:%.*]] = function_ref @$sSo17ReadWriteIntArrayV19__operatorSubscriptySpys5Int32VGAEFTo : $@convention(cxx_method) (Int32, @inout ReadWriteIntArray) -> UnsafeMutablePointer<Int32>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[INDEX]], [[SELFACCESS]]) : $@convention(cxx_method) (Int32, @inout ReadWriteIntArray) -> UnsafeMutablePointer<Int32>
// CHECK:   end_access [[SELFACCESS]] : $*ReadWriteIntArray
// CHECK:   [[PTR2:%.*]] = struct_extract [[PTR]] : $UnsafeMutablePointer<Int32>, #UnsafeMutablePointer._rawValue
// CHECK:   pointer_to_address [[PTR2]] : $Builtin.RawPointer to [strict] $*Int32
// CHECK: } // end sil function '$sSo17ReadWriteIntArrayVys5Int32VADcis'

public func index(_ arr: inout NonTrivialIntArrayByVal, _ arg: Int32, _ val: Int32) -> Int32 { arr[arg] }

// CHECK: sil @$s4main5indexys5Int32VSo23NonTrivialIntArrayByValVz_A2DtF : $@convention(thin) (@inout NonTrivialIntArrayByVal, Int32, Int32) -> Int32 {
// CHECK: bb0([[ARR:%.*]] : $*NonTrivialIntArrayByVal, [[INDEX:%.*]] : $Int32, [[NEWVALUE:%.*]] : $Int32):
// CHECK:   [[OP:%.*]] = function_ref @$sSo23NonTrivialIntArrayByValV24__operatorSubscriptConstys5Int32VAEFTo : $@convention(cxx_method) (Int32, @in_guaranteed NonTrivialIntArrayByVal) -> Int32
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[INDEX]], [[ARRACCESS:%.*]]) : $@convention(cxx_method) (Int32, @in_guaranteed NonTrivialIntArrayByVal) -> Int32
// CHECK: } // end sil function '$s4main5indexys5Int32VSo23NonTrivialIntArrayByValVz_A2DtF'

// CHECK: sil shared [transparent] @$sSo23NonTrivialIntArrayByValVys5Int32VADcig : $@convention(method) (Int32, @in_guaranteed NonTrivialIntArrayByVal) -> Int32 {
// CHECK: bb0([[NEWVALUE:%.*]] : $Int32, [[INDEX:%.*]] : $*NonTrivialIntArrayByVal):
// CHECK:   [[OP:%.*]] = function_ref @$sSo23NonTrivialIntArrayByValV24__operatorSubscriptConstys5Int32VAEFTo : $@convention(cxx_method) (Int32, @in_guaranteed NonTrivialIntArrayByVal) -> Int32
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[NEWVALUE]], [[SELFACCESS:%.*]]) : $@convention(cxx_method) (Int32, @in_guaranteed NonTrivialIntArrayByVal) -> Int32
// CHECK: } // end sil function '$sSo23NonTrivialIntArrayByValVys5Int32VADcig

public func index(_ arr: inout PtrByVal, _ arg: Int32, _ val: Int32) -> Int32 { arr[arg]![0] }
// CHECK: sil @$s4main5indexys5Int32VSo8PtrByValVz_A2DtF : $@convention(thin) (@inout PtrByVal, Int32, Int32) -> Int32 {
// CHECK: bb0([[ARR:%.*]] : $*PtrByVal, [[INDEX:%.*]] : $Int32, [[NEWVALUE:%.*]] : $Int32):
// CHECK:   [[ARRACCESS:%.*]] = begin_access [modify] [static] [[ARR]] : $*PtrByVal
// CHECK:   [[ARRACCESS2:%.*]] = begin_access [modify] [static] [[ARRACCESS]] : $*PtrByVal
// CHECK:   [[OP:%.*]] = function_ref @$sSo8PtrByValV19__operatorSubscriptySpys5Int32VGSgAEFTo : $@convention(cxx_method) (Int32, @inout PtrByVal) -> Optional<UnsafeMutablePointer<Int32>>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[INDEX]], [[ARRACCESS2]]) : $@convention(cxx_method) (Int32, @inout PtrByVal) -> Optional<UnsafeMutablePointer<Int32>>
// CHECK: } // end sil function '$s4main5indexys5Int32VSo8PtrByValVz_A2DtF'

// CHECK: sil shared [transparent] @$sSo8PtrByValVySpys5Int32VGSgADcig : $@convention(method) (Int32, @inout PtrByVal) -> Optional<UnsafeMutablePointer<Int32>> {
// CHECK: bb0([[NEWVALUE:%.*]] : $Int32, [[INDEX:%.*]] : $*PtrByVal):
// CHECK:   [[SELFACCESS:%.*]] = begin_access [modify] [static] [[INDEX]] : $*PtrByVal
// CHECK:   [[OP:%.*]] = function_ref @$sSo8PtrByValV19__operatorSubscriptySpys5Int32VGSgAEFTo : $@convention(cxx_method) (Int32, @inout PtrByVal) -> Optional<UnsafeMutablePointer<Int32>>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[NEWVALUE]], [[SELFACCESS]]) : $@convention(cxx_method) (Int32, @inout PtrByVal) -> Optional<UnsafeMutablePointer<Int32>>
// CHECK:   end_access [[SELFACCESS]] : $*PtrByVal
// CHECK: } // end sil function '$sSo8PtrByValVySpys5Int32VGSgADcig

public func index(_ arr: inout RefToPtr, _ arg: Int32, _ val: Int32) -> Int32 { arr[arg]![0] }
// CHECK: sil @$s4main5indexys5Int32VSo8RefToPtrVz_A2DtF : $@convention(thin) (@inout RefToPtr, Int32, Int32) -> Int32 {
// CHECK: bb0([[ARR:%.*]] : $*RefToPtr, [[INDEX:%.*]] : $Int32, [[NEWVALUE:%.*]] : $Int32):
// CHECK:   [[ARRACCESS:%.*]] = begin_access [modify] [static] [[ARR]] : $*RefToPtr
// CHECK:   [[ARRACCESS2:%.*]] = begin_access [modify] [static] [[ARRACCESS]] : $*RefToPtr
// CHECK:   [[OP:%.*]] = function_ref @$sSo8RefToPtrV19__operatorSubscriptySpySpys5Int32VGSgGAEFTo : $@convention(cxx_method) (Int32, @inout RefToPtr) -> UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int32>>>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[INDEX]], [[ARRACCESS2]]) : $@convention(cxx_method) (Int32, @inout RefToPtr) -> UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int32>>>
// CHECK: } // end sil function '$s4main5indexys5Int32VSo8RefToPtrVz_A2DtF'

// CHECK: sil shared [transparent] @$sSo8RefToPtrVySpys5Int32VGSgADcig : $@convention(method) (Int32, @inout RefToPtr) -> Optional<UnsafeMutablePointer<Int32>> {
// CHECK: bb0([[NEWVALUE:%.*]] : $Int32, [[INDEX:%.*]] : $*RefToPtr):
// CHECK:   [[SELFACCESS:%.*]] = begin_access [modify] [static] [[INDEX]] : $*RefToPtr
// CHECK:   [[OP:%.*]] = function_ref @$sSo8RefToPtrV19__operatorSubscriptySpySpys5Int32VGSgGAEFTo : $@convention(cxx_method) (Int32, @inout RefToPtr) -> UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int32>>>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[NEWVALUE]], [[SELFACCESS]]) : $@convention(cxx_method) (Int32, @inout RefToPtr) -> UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int32>>>
// CHECK:   end_access [[SELFACCESS]] : $*RefToPtr
// CHECK: } // end sil function '$sSo8RefToPtrVySpys5Int32VGSgADcig

public func index(_ arr: inout PtrToPtr, _ arg: Int32, _ val: Int32) -> Int32 { arr[arg]![0]![0] }
// CHECK: sil @$s4main5indexys5Int32VSo05PtrToD0Vz_A2DtF : $@convention(thin) (@inout PtrToPtr, Int32, Int32) -> Int32 {
// CHECK: bb0([[ARR:%.*]] : $*PtrToPtr, [[INDEX:%.*]] : $Int32, [[NEWVALUE:%.*]] : $Int32):
// CHECK:   [[ARRACCESS:%.*]] = begin_access [modify] [static] [[ARR]] : $*PtrToPtr
// CHECK:   [[ARRACCESS2:%.*]] = begin_access [modify] [static] [[ARRACCESS]] : $*PtrToPtr
// CHECK:   [[OP:%.*]] = function_ref @$sSo05PtrToA0V19__operatorSubscriptySpySpys5Int32VGSgGSgAEFTo : $@convention(cxx_method) (Int32, @inout PtrToPtr) -> Optional<UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int32>>>>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[INDEX]], [[ARRACCESS2]]) : $@convention(cxx_method) (Int32, @inout PtrToPtr) -> Optional<UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int32>>>>
// CHECK: } // end sil function '$s4main5indexys5Int32VSo05PtrToD0Vz_A2DtF'

// CHECK: sil shared [transparent] @$sSo05PtrToA0VySpySpys5Int32VGSgGSgADcig : $@convention(method) (Int32, @inout PtrToPtr) -> Optional<UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int32>>>> {
// CHECK: bb0([[NEWVALUE:%.*]] : $Int32, [[INDEX:%.*]] : $*PtrToPtr):
// CHECK:   [[SELFACCESS:%.*]] = begin_access [modify] [static] [[INDEX]] : $*PtrToPtr
// CHECK:   [[OP:%.*]] = function_ref @$sSo05PtrToA0V19__operatorSubscriptySpySpys5Int32VGSgGSgAEFTo : $@convention(cxx_method) (Int32, @inout PtrToPtr) -> Optional<UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int32>>>>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[NEWVALUE]], [[SELFACCESS]]) : $@convention(cxx_method) (Int32, @inout PtrToPtr) -> Optional<UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int32>>>>
// CHECK:   end_access [[SELFACCESS]] : $*PtrToPtr
// CHECK: } // end sil function '$sSo05PtrToA0VySpySpys5Int32VGSgGSgADcig

public func index(_ arr: ConstOpPtrByVal, _ arg: Int32, _ val: Int32) -> Int32 { arr[arg]![0] }
// CHECK: sil @$s4main5indexys5Int32VSo15ConstOpPtrByValV_A2DtF : $@convention(thin) (ConstOpPtrByVal, Int32, Int32) -> Int32 {
// CHECK: bb0([[ARR:%.*]] : $ConstOpPtrByVal, [[INDEX:%.*]] : $Int32, [[NEWVALUE:%.*]] : $Int32):
// CHECK:   [[OP:%.*]] = function_ref @$sSo15ConstOpPtrByValV019__operatorSubscriptA0ySPys5Int32VGSgAEFTo : $@convention(cxx_method) (Int32, @in_guaranteed ConstOpPtrByVal) -> Optional<UnsafePointer<Int32>>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[INDEX]], [[ARRACCESS2:%.*]]) : $@convention(cxx_method) (Int32, @in_guaranteed ConstOpPtrByVal) -> Optional<UnsafePointer<Int32>>
// CHECK: } // end sil function '$s4main5indexys5Int32VSo15ConstOpPtrByValV_A2DtF'

// CHECK: sil shared [transparent] @$sSo15ConstOpPtrByValVySPys5Int32VGSgADcig : $@convention(method) (Int32, ConstOpPtrByVal) -> Optional<UnsafePointer<Int32>> {
// CHECK: bb0([[NEWVALUE:%.*]] : $Int32, [[INDEX:%.*]] : $ConstOpPtrByVal):
// CHECK:   [[OP:%.*]] = function_ref @$sSo15ConstOpPtrByValV019__operatorSubscriptA0ySPys5Int32VGSgAEFTo : $@convention(cxx_method) (Int32, @in_guaranteed ConstOpPtrByVal) -> Optional<UnsafePointer<Int32>>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[NEWVALUE]], [[SELFACCESS:%.*]]) : $@convention(cxx_method) (Int32, @in_guaranteed ConstOpPtrByVal) -> Optional<UnsafePointer<Int32>>
// CHECK: } // end sil function '$sSo15ConstOpPtrByValVySPys5Int32VGSgADcig

public func index(_ arr: inout ConstPtrByVal, _ arg: Int32, _ val: Int32) -> Int32 { arr[arg]![0] }
// CHECK: sil @$s4main5indexys5Int32VSo13ConstPtrByValVz_A2DtF : $@convention(thin) (@inout ConstPtrByVal, Int32, Int32) -> Int32 {
// CHECK: bb0([[ARR:%.*]] : $*ConstPtrByVal, [[INDEX:%.*]] : $Int32, [[NEWVALUE:%.*]] : $Int32):
// CHECK:   [[ARRACCESS:%.*]] = begin_access [modify] [static] [[ARR]] : $*ConstPtrByVal
// CHECK:   [[ARRACCESS2:%.*]] = begin_access [modify] [static] [[ARRACCESS]] : $*ConstPtrByVal
// CHECK:   [[OP:%.*]] = function_ref @$sSo13ConstPtrByValV019__operatorSubscriptA0ySPys5Int32VGSgAEFTo : $@convention(cxx_method) (Int32, @inout ConstPtrByVal) -> Optional<UnsafePointer<Int32>>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[INDEX]], [[ARRACCESS2]]) : $@convention(cxx_method) (Int32, @inout ConstPtrByVal) -> Optional<UnsafePointer<Int32>>
// CHECK: } // end sil function '$s4main5indexys5Int32VSo13ConstPtrByValVz_A2DtF'

// CHECK: sil shared [transparent] @$sSo13ConstPtrByValVySPys5Int32VGSgADcig : $@convention(method) (Int32, @inout ConstPtrByVal) -> Optional<UnsafePointer<Int32>> {
// CHECK: bb0([[NEWVALUE:%.*]] : $Int32, [[INDEX:%.*]] : $*ConstPtrByVal):
// CHECK:   [[SELFACCESS:%.*]] = begin_access [modify] [static] [[INDEX]] : $*ConstPtrByVal
// CHECK:   [[OP:%.*]] = function_ref @$sSo13ConstPtrByValV019__operatorSubscriptA0ySPys5Int32VGSgAEFTo : $@convention(cxx_method) (Int32, @inout ConstPtrByVal) -> Optional<UnsafePointer<Int32>>
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[NEWVALUE]], [[SELFACCESS]]) : $@convention(cxx_method) (Int32, @inout ConstPtrByVal) -> Optional<UnsafePointer<Int32>>
// CHECK:   end_access [[SELFACCESS]] : $*ConstPtrByVal
// CHECK: } // end sil function '$sSo13ConstPtrByValVySPys5Int32VGSgADcig

public func subscriptUnnamed(_ unnamed: SubscriptUnnamedParameter, _ arg: Int32) -> Int32 { unnamed[arg] }
// CHECK: sil shared [transparent] @$sSo25SubscriptUnnamedParameterVys5Int32VADcig : $@convention(method) (Int32, SubscriptUnnamedParameter) -> Int32 {
// CHECK: bb0([[INDEX:%.*]] : $Int32, [[SELF:%.*]] : $SubscriptUnnamedParameter):
// CHECK:   [[SELFACCESS:%.*]] = alloc_stack $SubscriptUnnamedParameter
// CHECK:   [[OP:%.*]] = function_ref @$sSo25SubscriptUnnamedParameterV010__operatorA5Constys5Int32VAEFTo : $@convention(cxx_method) (Int32, @in_guaranteed SubscriptUnnamedParameter) -> Int32
// CHECK:   [[PTR:%.*]] = apply [[OP]]([[INDEX]], [[SELFACCESS]]) : $@convention(cxx_method) (Int32, @in_guaranteed SubscriptUnnamedParameter) -> Int32
// CHECK:   dealloc_stack [[SELFACCESS]]
// CHECK: } // end sil function '$sSo25SubscriptUnnamedParameterVys5Int32VADcig'

// CHECK: sil [asmname "{{.*}}ReadOnlyIntArray{{.*}}"] [clang ReadOnlyIntArray.__operatorSubscriptConst] @$sSo16ReadOnlyIntArrayV24__operatorSubscriptConstySPys5Int32VGAEFTo : $@convention(cxx_method) (Int32, @in_guaranteed ReadOnlyIntArray) -> UnsafePointer<Int32>
// CHECK: sil [asmname "{{.*}}ReadWriteIntArray{{.*}}"] [clang ReadWriteIntArray.__operatorSubscript] @$sSo17ReadWriteIntArrayV19__operatorSubscriptySpys5Int32VGAEFTo : $@convention(cxx_method) (Int32, @inout ReadWriteIntArray) -> UnsafeMutablePointer<Int32>
// CHECK: sil [asmname "{{.*}}NonTrivialIntArrayByVal{{.*}}"] [clang NonTrivialIntArrayByVal.__operatorSubscriptConst] @$sSo23NonTrivialIntArrayByValV24__operatorSubscriptConstys5Int32VAEFTo : $@convention(cxx_method) (Int32, @in_guaranteed NonTrivialIntArrayByVal) -> Int32

// CHECK: sil [asmname "{{.*}}PtrByVal{{.*}}"] [clang PtrByVal.__operatorSubscript] @$sSo8PtrByValV19__operatorSubscriptySpys5Int32VGSgAEFTo : $@convention(cxx_method) (Int32, @inout PtrByVal) -> Optional<UnsafeMutablePointer<Int32>>
// CHECK: sil [asmname "{{.*}}RefToPtr{{.*}}"] [clang RefToPtr.__operatorSubscript] @$sSo8RefToPtrV19__operatorSubscriptySpySpys5Int32VGSgGAEFTo : $@convention(cxx_method) (Int32, @inout RefToPtr) -> UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int32>>>
// CHECK: sil [asmname "{{.*}}PtrToPtr{{.*}}"] [clang PtrToPtr.__operatorSubscript] @$sSo05PtrToA0V19__operatorSubscriptySpySpys5Int32VGSgGSgAEFTo : $@convention(cxx_method) (Int32, @inout PtrToPtr) -> Optional<UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int32>>>>
// CHECK: sil [asmname "{{.*}}ConstOpPtrByVal{{.*}}"] [clang ConstOpPtrByVal.__operatorSubscriptConst] @$sSo15ConstOpPtrByValV019__operatorSubscriptA0ySPys5Int32VGSgAEFTo : $@convention(cxx_method) (Int32, @in_guaranteed ConstOpPtrByVal) -> Optional<UnsafePointer<Int32>>
// CHECK: sil [asmname "{{.*}}ConstPtrByVal{{.*}}"] [clang ConstPtrByVal.__operatorSubscriptConst] @$sSo13ConstPtrByValV019__operatorSubscriptA0ySPys5Int32VGSgAEFTo : $@convention(cxx_method) (Int32, @inout ConstPtrByVal) -> Optional<UnsafePointer<Int32>>
