// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -emit-module -o %t -module-name Lib -I %S/Inputs/custom-modules -swift-version 4 %s

// RUN: %target-swift-ide-test -source-filename=x -print-module -module-to-print Lib -I %t -I %S/Inputs/custom-modules -swift-version 4 | %FileCheck %s

// RUN: %target-swift-ide-test -source-filename=x -print-module -module-to-print Lib -I %t -I %S/Inputs/custom-modules -enable-experimental-deserialization-recovery -swift-version 3 | %FileCheck -check-prefix CHECK-RECOVERY %s

// RUN: %target-swift-frontend -typecheck %s -I %t -I %S/Inputs/custom-modules  -swift-version 3 -enable-experimental-deserialization-recovery -D TEST -verify

// REQUIRES: objc_interop

#if TEST

import Lib

func requiresConformance(_: B_RequiresConformance<B_ConformsToProto>) {}


#else // TEST

import Types

// Please use prefixes to keep the printed parts of this file in alphabetical
// order.

public func A_renameAllTheThings(
  a: RenamedClass?,
  b: RenamedGenericClass<AnyObject>?,
  c: RenamedTypedef,
  d: RenamedStruct,
  e: RenamedEnum,
  f: RenamedProtocol
) {}

// CHECK-LABEL: func A_renameAllTheThings(
// CHECK-SAME: a: RenamedClass?
// CHECK-SAME: b: RenamedGenericClass<AnyObject>?
// CHECK-SAME: c: RenamedTypedef
// CHECK-SAME: d: RenamedStruct
// CHECK-SAME: e: RenamedEnum
// CHECK-SAME: f: RenamedProtocol
// CHECK-SAME: )


// CHECK-RECOVERY-LABEL: func A_renameAllTheThings(
// CHECK-RECOVERY-SAME: a: Swift3RenamedClass?
// CHECK-RECOVERY-SAME: b: Swift3RenamedGenericClass<AnyObject>?

// Unfortunately we can't use the presence of a forwarding typealias to
// automatically look for the new name, because it's /already/ a typealias.
// CHECK-RECOVERY-SAME: c: RenamedTypedef

// CHECK-RECOVERY-SAME: d: Swift3RenamedStruct
// CHECK-RECOVERY-SAME: e: Swift3RenamedEnum
// CHECK-RECOVERY-SAME: f: Swift3RenamedProtocol
// CHECK-RECOVERY-SAME: )


public func A_renameGeneric<T: RenamedProtocol>(obj: T) {}

// CHECK-LABEL: func A_renameGeneric<T>(
// CHECK-SAME: where T : RenamedProtocol

// CHECK-RECOVERY-LABEL: func A_renameGeneric<T>(
// CHECK-RECOVERY-SAME: where T : Swift3RenamedProtocol

public class B_ConformsToProto: RenamedProtocol {}

// CHECK-LABEL: class B_ConformsToProto : RenamedProtocol
// CHECK-RECOVERY-LABEL: class B_ConformsToProto : Swift3RenamedProtocol

public struct B_RequiresConformance<T: RenamedProtocol> {}

// CHECK-LABEL: struct B_RequiresConformance<T> where T : RenamedProtocol
// CHECK-RECOVERY-LABEL: struct B_RequiresConformance<T> where T : Swift3RenamedProtocol

#endif
