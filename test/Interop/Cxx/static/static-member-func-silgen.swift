// RUN: %target-swift-emit-sil -Xllvm -sil-print-types %s -I %S/Inputs -enable-experimental-cxx-interop | %FileCheck %s

import StaticMemberFunc

func callStaticMemberFunc() -> CInt {
  return WithStaticMemberFunc.staticMemberFunc()
}

// CHECK: sil hidden @$s4main20callStaticMemberFuncs5Int32VyF : $@convention(thin) () -> Int32 {
// CHECK: [[FUNC:%.*]] = function_ref @$sSo20WithStaticMemberFuncV06staticcD0s5Int32VyFZTo : $@convention(c) () -> Int32
// CHECK: [[VALUE:%.*]] = apply [[FUNC]]() : $@convention(c) () -> Int32
// CHECK: return [[VALUE]] : $Int32

// CHECK: // clang name: WithStaticMemberFunc::staticMemberFunc
// CHECK: sil [asmname "{{.*}}staticMemberFunc{{.*}}"] [clang WithStaticMemberFunc.staticMemberFunc] @$sSo20WithStaticMemberFuncV06staticcD0s5Int32VyFZTo : $@convention(c) () -> Int32

func callStaticMemberFuncAddr() -> CInt {
  return WithStaticMemberFunc.getStaticMemberFuncAddress()!()
}

// CHECK: sil hidden @$s4main24callStaticMemberFuncAddrs5Int32VyF : $@convention(thin) () -> Int32
// CHECK: [[FUNC:%.*]] = function_ref @$sSo20WithStaticMemberFuncV03getbcD7Addresss5Int32VyXCSgyFZTo : $@convention(c) () -> Optional<@convention(c) () -> Int32>
// CHECK: [[VALUE:%.*]] = apply [[FUNC]]() : $@convention(c) () -> Optional<@convention(c) () -> Int32>

// CHECK: // clang name: WithStaticMemberFunc::getStaticMemberFuncAddress
// CHECK: sil [asmname "{{.*}}getStaticMemberFuncAddress{{.*}}"] [clang WithStaticMemberFunc.getStaticMemberFuncAddress] @$sSo20WithStaticMemberFuncV03getbcD7Addresss5Int32VyXCSgyFZTo : $@convention(c) () -> Optional<@convention(c) () -> Int32>
