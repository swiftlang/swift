// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -I %S/Inputs -enable-experimental-cxx-interop %s | %FileCheck %s

// REQUIRES: swift_in_compiler

// CHECK: // clang name: WithStaticMember::staticMember
// CHECK: sil_global public_external @{{_ZN16WithStaticMember12staticMemberE|\?staticMember@WithStaticMember@@2HA}} : $Int32
// CHECK: // clang name: WithIncompleteStaticMember::selfMember
// CHECK: sil_global public_external @{{_ZN26WithIncompleteStaticMember10selfMemberE|\?selfMember@WithIncompleteStaticMember@@2V1@A}} : $WithIncompleteStaticMember
import StaticMemberVar

func readStaticMember() -> CInt {
  return WithStaticMember.staticMember
}

// CHECK: sil hidden @$s4main16readStaticMembers5Int32VyF : $@convention(thin) () -> Int32
// CHECK: [[ADDR:%.*]] = global_addr @{{_ZN16WithStaticMember12staticMemberE|\?staticMember@WithStaticMember@@2HA}} : $*Int32
// CHECK: [[ACCESS:%.*]] = begin_access [read] [dynamic] [[ADDR]] : $*Int32
// CHECK: [[VALUE:%.*]] = load [[ACCESS]] : $*Int32
// CHECK: return [[VALUE]] : $Int32

func writeStaticMember() {
  WithStaticMember.staticMember = -1
}

// CHECK: sil hidden @$s4main17writeStaticMemberyyF : $@convention(thin) () -> ()
// CHECK: [[ADDR:%.*]] = global_addr @{{_ZN16WithStaticMember12staticMemberE|\?staticMember@WithStaticMember@@2HA}} : $*Int32
// CHECK: [[LIT:%.*]] = integer_literal $Builtin.Int32, -1
// CHECK: [[INT:%.*]] = struct $Int32 ([[LIT]] : $Builtin.Int32)
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[ADDR]] : $*Int32
// CHECK: store [[INT]] to [[ACCESS]] : $*Int32

func readSelfMember() -> WithIncompleteStaticMember {
  return WithIncompleteStaticMember.selfMember
}

// CHECK: sil hidden @$s4main14readSelfMemberSo020WithIncompleteStaticD0VyF : $@convention(thin) () -> WithIncompleteStaticMember
// CHECK: [[ADDR:%.*]] = global_addr @{{_ZN26WithIncompleteStaticMember10selfMemberE|\?selfMember@WithIncompleteStaticMember@@2V1@A}} : $*WithIncompleteStaticMember
// CHECK: [[ACCESS:%.*]] = begin_access [read] [dynamic] [[ADDR]] : $*WithIncompleteStaticMember
// CHECK: [[VALUE:%.*]] = load [[ACCESS]] : $*WithIncompleteStaticMember
// CHECK: return [[VALUE]] : $WithIncompleteStaticMember

func writeSelfMember(_ m: WithIncompleteStaticMember) {
  WithIncompleteStaticMember.selfMember = m
}

// CHECK: sil hidden @$s4main15writeSelfMemberyySo020WithIncompleteStaticD0VF : $@convention(thin) (WithIncompleteStaticMember) -> ()
// CHECK: [[ADDR:%.*]] = global_addr @{{_ZN26WithIncompleteStaticMember10selfMemberE|\?selfMember@WithIncompleteStaticMember@@2V1@A}} : $*WithIncompleteStaticMember
// CHECK: [[ACCESS:%.*]] = begin_access [modify] [dynamic] [[ADDR]] : $*WithIncompleteStaticMember
// CHECK:  store %0 to [[ACCESS]] : $*WithIncompleteStaticMember
//TODO fix undefined reference to `WithConstStaticMember::notDefined`.
// func readNotDefinedConstMember() -> CInt {
//   return WithConstStaticMember.notDefined
// }
func readDefinedConstMember() -> CInt {
  return WithConstStaticMember.defined
}

// CHECK: sil hidden @$s4main22readDefinedConstMembers5Int32VyF : $@convention(thin) () -> Int32
// CHECK: [[VALUE:%.*]] = integer_literal $Builtin.Int32, 48
// CHECK: [[STRUCT:%.*]] = struct $Int32 ([[VALUE]] : $Builtin.Int32)
// CHECK: return [[STRUCT]] : $Int32

func readDefinedOutOfLineConstMember() -> CInt {
  return WithConstStaticMember.definedOutOfLine
}