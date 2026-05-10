// RUN: %empty-directory(%t) 

// 1. functional test:

// RUN: %target-build-swift -parse-as-library -wmo -emit-module -emit-module-path=%t/Classes.swiftmodule -module-name=Classes %S/Inputs/classes.swift -c -o %t/classes.o
// RUN: %target-build-swift -parse-as-library -wmo -enable-library-evolution -emit-module -emit-module-path=%t/ResilientClasses.swiftmodule -module-name=ResilientClasses %S/Inputs/classes.swift -c -o %t/resilientclasses.o
// RUN: %target-build-swift -wmo -module-name=Main -I%t %s -c -o %t/main.o
// RUN: %target-swiftc_driver %t/main.o %t/classes.o %t/resilientclasses.o -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-OUTPUT

// 2. check if the generated IR looks like expected:

// RUN: %target-swift-frontend -module-name=Main -I%t %s -emit-ir -g -o - | %FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-%target-objc-interop

// REQUIRES: executable_test


import Classes
import ResilientClasses

final class Internal : Classes.OpenBase, Hashable {
  func hash(into hasher: inout Hasher) {}
  static func == (lhs: Internal, rhs: Internal) -> Bool { return false }
}

final class DerivedFromResilient : ResilientClasses.OpenBase {
}

final class Generic<T> : Classes.OpenBase {
   final class Inner : Classes.OpenBase {}
}

// CHECK-LABEL: define {{.*}} @"$s4Main14castToNonfinaly7Classes0D0CSgAC4BaseCF"
// CHECK:         @swift_dynamicCastClass
// CHECK:       }
@inline(never)
func castToNonfinal(_ b: Classes.Base) -> Classes.Nonfinal? {
  return b as? Classes.Nonfinal
}

// CHECK-LABEL: define {{.*}} @"$s4Main11castToFinaly7Classes0D0CSgAC4BaseCF"
// CHECK-NOT:     @swift_dynamicCastClass
// CHECK:       }
@inline(never)
func castToFinal(_ b: Classes.Base) -> Classes.Final? {
  return b as? Classes.Final
}

// CHECK-LABEL: define {{.*}} @"$s4Main24unconditionalCastToFinaly7Classes0E0CAC4BaseCF"
// CHECK-NOT:     call {{.*}}@object_getClass
// CHECK-NOT:     @swift_dynamicCastClass
// CHECK:       }
@inline(never)
func unconditionalCastToFinal(_ b: Classes.Base) -> Classes.Final {
  return b as! Classes.Final
}

// CHECK-LABEL: define {{.*}} @"$s4Main32unconditionalOptionalCastToFinaly7Classes0F0CAC4BaseCSgF"
// CHECK-NOT:     call {{.*}}@object_getClass
// CHECK-NOT:     @swift_dynamicCastClass
// CHECK:       }
@inline(never)
func unconditionalOptionalCastToFinal(_ b: Classes.Base?) -> Classes.Final {
  return b as! Classes.Final
}

// CHECK-LABEL: define {{.*}} @"$s4Main20castToResilientFinaly0D7Classes0E0CSgAC4BaseCF"
// CHECK:         @swift_dynamicCastClass
// CHECK:       }
@inline(never)
func castToResilientFinal(_ b: ResilientClasses.Base) -> ResilientClasses.Final? {
  return b as? ResilientClasses.Final
}

// CHECK-LABEL: define {{.*}} @"$s4Main19castProtocolToFinaly7Classes0E0CSgAC1P_pF"
// CHECK-objc:    call {{.*}}@object_getClass
// CHECK-nonobjc: load
// CHECK-NOT:     @swift_dynamicCastClass
// CHECK:       }
@inline(never)
func castProtocolToFinal(_ p: Classes.P) -> Classes.Final? {
  return p as? Classes.Final
}

// CHECK-LABEL: define {{.*}} @"$s4Main14castToInternalyAA0D0CSg7Classes8OpenBaseCF"
// CHECK-NOT:     @swift_dynamicCastClass
// CHECK:       }
@inline(never)
func castToInternal(_ b: Classes.OpenBase) -> Internal? {
  return b as? Internal
}

// CHECK-LABEL: define {{.*}} @"$s4Main23castAnyObjectToInternalyAA0F0CSgyXlF"
// CHECK:         @swift_dynamicCastClass
// CHECK:       }
@inline(never)
func castAnyObjectToInternal(_ a: AnyObject) -> Internal? {
  return a as? Internal
}

// CHECK-LABEL: define {{.*}} @"$s4Main26castToDerivedFromResilientyAA0deF0CSg0F7Classes8OpenBaseCF"
// CHECK:         @swift_dynamicCastClass
// CHECK:       }
@inline(never)
func castToDerivedFromResilient(_ b: ResilientClasses.OpenBase) -> DerivedFromResilient? {
  return b as? DerivedFromResilient
}

// CHECK-LABEL: define {{.*}} @"$s4Main13castToGenericyAA0D0CySiGSg7Classes8OpenBaseCF"
// CHECK:         @swift_dynamicCastClass
// CHECK:       }
@inline(never)
func castToGeneric(_ b: Classes.OpenBase) -> Generic<Int>? {
  return b as? Generic<Int>
}

// CHECK-LABEL: define {{.*}} @"$s4Main18castToGenericInneryAA0D0C0E0CySi_GSg7Classes8OpenBaseCF"
// CHECK:         @swift_dynamicCastClass
// CHECK:       }
@inline(never)
func castToGenericInner(_ b: Classes.OpenBase) -> Generic<Int>.Inner? {
  return b as? Generic<Int>.Inner
}


// CHECK-LABEL: define {{.*}} @"$s4Main14getAnyHashableys0cD0VAA8InternalCF"
@inline(never)
func getAnyHashable(_ i: Internal) -> AnyHashable {
  return i
}

func test() {
  // CHECK-OUTPUT: nil
  print(castToNonfinal(Classes.Base()) as Any)
  // CHECK-OUTPUT: Optional(Classes.Nonfinal)
  print(castToNonfinal(Classes.Nonfinal()) as Any)

  // CHECK-OUTPUT: nil
  print(castToFinal(Classes.Base()) as Any)
  // CHECK-OUTPUT: Optional(Classes.Final)
  print(castToFinal(Classes.Final()) as Any)
  // CHECK-OUTPUT: Classes.Final
  print(unconditionalCastToFinal(Classes.Final()))
  // CHECK-OUTPUT: Classes.Final
  print(unconditionalOptionalCastToFinal(Classes.Final()))

  // CHECK-OUTPUT: nil
  print(castToResilientFinal(ResilientClasses.Base()) as Any)
  // CHECK-OUTPUT: Optional(ResilientClasses.Final)
  print(castToResilientFinal(ResilientClasses.Final()) as Any)

  // CHECK-OUTPUT: nil
  print(castProtocolToFinal(Classes.Nonfinal()) as Any)
  // CHECK-OUTPUT: Optional(Classes.Final)
  print(castProtocolToFinal(Classes.Final()) as Any)

  // CHECK-OUTPUT: nil
  print(castToInternal(Classes.OpenBase()) as Any)
  // CHECK-OUTPUT: Optional(Main.Internal)
  print(castToInternal(Internal()) as Any)

  // CHECK-OUTPUT: nil
  print(castAnyObjectToInternal(Classes.OpenBase()) as Any)
  // CHECK-OUTPUT: Optional(Main.Internal)
  let i = Internal()
  print(castAnyObjectToInternal(i) as Any)
  let i2 = castAnyObjectToInternal(getAnyHashable(i) as! AnyObject)
  precondition(i === i2)

  // CHECK-OUTPUT: nil
  print(castToDerivedFromResilient(ResilientClasses.OpenBase()) as Any)
  // CHECK-OUTPUT: Optional(Main.DerivedFromResilient)
  print(castToDerivedFromResilient(DerivedFromResilient()) as Any)

  // CHECK-OUTPUT: nil
  print(castToGeneric(Classes.OpenBase()) as Any)
  // CHECK-OUTPUT: Optional(Main.Generic<Swift.Int>)
  print(castToGeneric(Generic<Int>()) as Any)

  // CHECK-OUTPUT: nil
  print(castToGenericInner(Classes.OpenBase()) as Any)
  // CHECK-OUTPUT: Optional(Main.Generic<Swift.Int>.Inner)
  print(castToGenericInner(Generic<Int>.Inner()) as Any)
}

test()

