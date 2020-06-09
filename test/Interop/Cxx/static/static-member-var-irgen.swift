// RUN: %target-swift-emit-ir -I %S/Inputs -enable-cxx-interop %s | %FileCheck %s

// CHECK: @{{_ZN16WithStaticMember12staticMemberE|"\?staticMember@WithStaticMember@@2HA"}} = external {{(dso_local )?}}global i32, align 4
// CHECK: @{{_ZN26WithIncompleteStaticMember10selfMemberE|"\?selfMember@WithIncompleteStaticMember@@2V1@A"}} = {{external|linkonce_odr}} {{(dso_local )?}}global %class.WithIncompleteStaticMember, align 4

//TODO: This test uses only values of static const members, so it does not need
//to depend on external definitions. However, our code generation pattern loads
//the value dynamically. Instead, we should inline known constants. That would
//allow Swift code to even read the value of WithIncompleteStaticMember::notDefined.
// CHECK: @{{_ZN21WithConstStaticMember7definedE|"\?defined@WithConstStaticMember@@2HB"}} = {{available_externally|linkonce_odr}} {{(dso_local )?}}constant i32 48, {{(comdat, )?}}align 4
// CHECK: @{{_ZN21WithConstStaticMember16definedOutOfLineE|"\?definedOutOfLine@WithConstStaticMember@@2HB"}} = external {{(dso_local )?}}constant i32, align 4

//TODO: This test uses only values of static const members, so it does not need
//to depend on external definitions. However, our code generation pattern loads
//the value dynamically. Instead, we should inline known constants. That would
//allow Swift code to even read the value of WithIncompleteStaticMember::notDefined.
// CHECK: @{{_ZN25WithConstexprStaticMember13definedInlineE|"\?definedInline@WithConstexprStaticMember@@2HB"}} = linkonce_odr {{(dso_local )?}}constant i32 139, {{(comdat, )?}}align 4

import StaticMemberVar

public func readStaticMember() -> CInt {
  return WithStaticMember.staticMember
}

// CHECK: define {{(protected |dllexport )?}}swiftcc i32 @"$s4main16readStaticMembers5Int32VyF"()
// CHECK: [[VALUE:%.*]] = load i32, i32* getelementptr inbounds (%Ts5Int32V, %Ts5Int32V* bitcast (i32* @{{_ZN16WithStaticMember12staticMemberE|"\?staticMember@WithStaticMember@@2HA"}} to %Ts5Int32V*), i32 0, i32 0), align 4
// CHECK: ret i32 [[VALUE]]

public func writeStaticMember() {
  WithStaticMember.staticMember = -1
}

// CHECK: define {{(protected |dllexport )?}}swiftcc void @"$s4main17writeStaticMemberyyF"() #0
// CHECK: store i32 -1, i32* getelementptr inbounds (%Ts5Int32V, %Ts5Int32V* bitcast (i32* @{{_ZN16WithStaticMember12staticMemberE|"\?staticMember@WithStaticMember@@2HA"}} to %Ts5Int32V*), i32 0, i32 0), align 4

public func readSelfMember() -> WithIncompleteStaticMember {
  return WithIncompleteStaticMember.selfMember
}

// CHECK: define {{(protected |dllexport )?}}swiftcc i32 @"$s4main14readSelfMemberSo020WithIncompleteStaticD0VyF"() #0
// CHECK: [[VALUE:%.*]] = load i32, i32* getelementptr inbounds (%TSo26WithIncompleteStaticMemberV, %TSo26WithIncompleteStaticMemberV* bitcast (%class.WithIncompleteStaticMember* @{{_ZN26WithIncompleteStaticMember1|"\?selfMember@WithIncompleteStaticMember@@2V1@A"}}
// CHECK: ret i32 [[VALUE]]

public func writeSelfMember(_ m: WithIncompleteStaticMember) {
  WithIncompleteStaticMember.selfMember = m
}

// CHECK: define {{(protected |dllexport )?}}swiftcc void @"$s4main15writeSelfMemberyySo020WithIncompleteStaticD0VF"(i32 %0) #0
// CHECK: store i32 %0, i32* getelementptr inbounds (%TSo26WithIncompleteStaticMemberV, %TSo26WithIncompleteStaticMemberV* bitcast (%class.WithIncompleteStaticMember* @{{_ZN26WithIncompleteStaticMember10selfMemberE|"\?selfMember@WithIncompleteStaticMember@@2V1@A"}} to %TSo26WithIncompleteStaticMemberV*), i32 0, i32 0, i32 0), align 4

// TODO: Currently, the generated code would try to load the value from
// a symbol that is not defined. We should inline the value instead.
// public func readNotDefinedConstMember() -> CInt {
//   return WithConstStaticMember.notDefined
// }

public func readDefinedConstMember() -> CInt {
  return WithConstStaticMember.defined
}

// CHECK: define {{(protected |dllexport )?}}swiftcc i32 @"$s4main22readDefinedConstMembers5Int32VyF"() #0
// CHECK: [[VALUE:%.*]] = load i32, i32* getelementptr inbounds (%Ts5Int32V, %Ts5Int32V* bitcast (i32* @{{_ZN21WithConstStaticMember7definedE|"\?defined@WithConstStaticMember@@2HB"}} to %Ts5Int32V*), i32 0, i32 0), align 4
// CHECK: ret i32 [[VALUE]]

public func readDefinedOutOfLineConstMember() -> CInt {
  return WithConstStaticMember.definedOutOfLine
}

// CHECK: define {{(protected |dllexport )?}}swiftcc i32 @"$s4main31readDefinedOutOfLineConstMembers5Int32VyF"() #0
// CHECK: [[VALUE:%.*]] = load i32, i32* getelementptr inbounds (%Ts5Int32V, %Ts5Int32V* bitcast (i32* @{{_ZN21WithConstStaticMember16definedOutOfLineE|"\?definedOutOfLine@WithConstStaticMember@@2HB"}} to %Ts5Int32V*), i32 0, i32 0), align 4
// CHECK: ret i32 [[VALUE]]

public func readConstexprStaticMember() -> CInt {
  return WithConstexprStaticMember.definedInline
}

// CHECK: define {{(protected |dllexport )?}}swiftcc i32 @"$s4main25readConstexprStaticMembers5Int32VyF"() #0
// CHECK: [[VALUE:%.*]] = load i32, i32* getelementptr inbounds (%Ts5Int32V, %Ts5Int32V* bitcast (i32* @{{_ZN25WithConstexprStaticMember13definedInlineE|"\?definedInline@WithConstexprStaticMember@@2HB"}} to %Ts5Int32V*), i32 0, i32 0), align 4
// CHECK: ret i32 [[VALUE]]
