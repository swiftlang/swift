// RUN: %target-swift-emit-sil -I %S/Inputs -enable-cxx-interop %s | %FileCheck %s

// CHECK: // clang name: WithStaticMember::staticMember
// CHECK: sil_global public_external @{{_ZN16WithStaticMember12staticMemberE|\?staticMember@WithStaticMember@@2HA}} : $Int32
// CHECK: // clang name: WithIncompleteStaticMember::selfMember
// CHECK: sil_global public_external @{{_ZN26WithIncompleteStaticMember10selfMemberE|\?selfMember@WithIncompleteStaticMember@@2V1@A}} : $WithIncompleteStaticMember
// CHECK: // clang name: WithConstStaticMember::defined
// CHECK: sil_global public_external [let] @{{_ZN21WithConstStaticMember7definedE|\?defined@WithConstStaticMember@@2HB}} : $Int32
// CHECK: // clang name: WithConstStaticMember::definedOutOfLine
// CHECK: sil_global public_external [let] @{{_ZN21WithConstStaticMember16definedOutOfLineE|\?definedOutOfLine@WithConstStaticMember@@2HB}} : $Int32

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
// CHECK: [[INT:%.*]] = struct $Int32 (%2 : $Builtin.Int32)
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
// CHECK: [[ADDR:%.*]] = global_addr @{{_ZN21WithConstStaticMember7definedE|\?defined@WithConstStaticMember@@2HB}} : $*Int32
// CHECK: [[VALUE:%.*]] = load [[ADDR]] : $*Int32
// CHECK: return [[VALUE]] : $Int32

func readDefinedOutOfLineConstMember() -> CInt {
  return WithConstStaticMember.definedOutOfLine
}

// CHECK: sil hidden @$s4main25readConstexprStaticMembers5Int32VyF : $@convention(thin) () -> Int32
// CHECK: [[META:%.*]] = metatype $@thin WithConstexprStaticMember.Type
// CHECK: [[ACC:%.*]] = function_ref @$sSo25WithConstexprStaticMemberV13definedInlines5Int32VvgZ : $@convention(method) (@thin WithConstexprStaticMember.Type) -> Int32
// CHECK: [[OUT:%.*]] = apply [[ACC]]([[META]]) : $@convention(method) (@thin WithConstexprStaticMember.Type) -> Int32
// CHECK: return [[OUT]] : $Int32
// CHECK-LABEL: end sil function '$s4main25readConstexprStaticMembers5Int32VyF'

// Make sure we also generate the accessor with a numeric literal.
// CHECK-LABEL: sil shared @$sSo25WithConstexprStaticMemberV13definedInlines5Int32VvgZ : $@convention(method) (@thin WithConstexprStaticMember.Type) -> Int32
// CHECK: [[IL:%.*]] = integer_literal $Builtin.Int32, 139
// CHECK: [[OUT:%.*]] = struct $Int32 ([[IL]] : $Builtin.Int32)
// CHECK: return [[OUT]] : $Int32
// CHECK-LABEL: end sil function '$sSo25WithConstexprStaticMemberV13definedInlines5Int32VvgZ'

func readConstexprStaticMember() -> CInt {
  return WithConstexprStaticMember.definedInline
}
