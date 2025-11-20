// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -I %S/Inputs -enable-experimental-cxx-interop %s | %FileCheck %s

import InlineStaticMemberVar

func readStaticMember() -> CInt {
  return WithInlineStaticMember.staticMember
}

// CHECK: // clang name: WithInlineStaticMember::staticMember
// CHECK: sil_global public_external @{{_ZN22WithInlineStaticMember12staticMemberE|\?staticMember@WithInlineStaticMember@@2HA}} : $Int32

// CHECK: sil hidden @$s4main16readStaticMembers5Int32VyF : $@convention(thin) () -> Int32
// CHECK: [[ADDR:%.*]] = global_addr @{{_ZN22WithInlineStaticMember12staticMemberE|\?staticMember@WithInlineStaticMember@@2HA}} : $*Int32
// CHECK: [[ACCESS:%.*]] = begin_access [read] [dynamic] [[ADDR]] : $*Int32
// CHECK: [[VALUE:%.*]] = load [[ACCESS]] : $*Int32
// CHECK: return [[VALUE]] : $Int32

func writeStaticMember(_ c: CInt) {
  WithInlineStaticMember.staticMember = c
}

// CHECK: sil hidden @$s4main17writeStaticMemberyys5Int32VF : $@convention(thin) (Int32) -> ()
// CHECK: [[ADDR:%.*]] = global_addr @{{_ZN22WithInlineStaticMember12staticMemberE|\?staticMember@WithInlineStaticMember@@2HA}} : $*Int32
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[ADDR]] : $*Int32
// CHECK: store %0 to [[ACCESS]] : $*Int32

func modifyInout(_ c: inout CInt) {
  c = 42
}

func passingVarAsInout() {
  modifyInout(&WithInlineStaticMember.staticMember)
}

// CHECK: sil hidden @$s4main17passingVarAsInoutyyF : $@convention(thin) () -> ()
// CHECK: [[ADDR:%.*]] = global_addr @{{_ZN22WithInlineStaticMember12staticMemberE|\?staticMember@WithInlineStaticMember@@2HA}} : $*Int32
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[ADDR]] : $*Int32
// CHECK: [[FUNC:%.*]] = function_ref @$s4main11modifyInoutyys5Int32VzF : $@convention(thin) (@inout Int32) -> ()
// CHECK: apply [[FUNC]]([[ACCESS]]) : $@convention(thin) (@inout Int32) -> ()
