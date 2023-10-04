// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Utils.swift \
// RUN:   -module-name Utils -swift-version 5 -I %t \
// RUN:   -package-name mypkg \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module -emit-module-path %t/Utils.swiftmodule

// RUN: %target-swift-frontend -emit-sil %t/Client.swift -package-name mypkg -I %t > %t/Client.sil
// RUN: %FileCheck %s < %t/Client.sil
// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t -swift-version 5 -package-name mypkg -verify

//--- Utils.swift

// Resilient; public.
// Switch stmt requires @unknown default.
public enum PublicEnum {
  case one
  case two(Int)
}

// Resilient; public.
public struct PublicStruct {
  public var publicVar: Int
}

// Non-resilient; frozen. Accessed directly.
// Switch stmt does not require @unknown default.
@frozen
public enum FrozenPublicEnum {
  case one
  case two(Int)
}

// Non-resilient; non-public / associated value is also non-resilient (Int is @frozen public).
// Accessed directly.
// Switch stmt does not require @unknown default.
package enum PkgEnum {
  case one
  case two(Int)
}

// Non-resilient but accessed indirectly since associated value is resilient.
// Passed by address to func as @in_guaranteed in Silgen.
// Switch stmt does not require @unknown default.
package enum PkgEnumWithPublicCase {
  case one
  case two(PublicStruct)
}

// Non-resilient but accessed indirectly since associated value is resilient (existential).
// Passed by address to func as @in_guaranteed in Silgen.
// Switch stmt does not require @unknown default.
package enum PkgEnumWithExistentialCase {
  case one
  case two(any StringProtocol)
}

// Resilient since inlinable.
@usableFromInline
package enum UfiPkgEnum {
  case one
  case two(Int)
}


//--- Client.swift
import Utils

package func f(_ arg: PkgEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

// CHECK: // f(_:)
// CHECK-NEXT: sil @$s6Client1fySi5Utils7PkgEnumOF : $@convention(thin) (PkgEnum) -> Int {
// CHECK-NEXT: // %0 "arg"                                       // users: %2, %1
// CHECK-NEXT: bb0(%0 : $PkgEnum):
// CHECK-NEXT:   debug_value %0 : $PkgEnum, let, name "arg", argno 1 // id: %1
// CHECK-NEXT:   switch_enum %0 : $PkgEnum, case #PkgEnum.one!enumelt: bb1, case #PkgEnum.two!enumelt: bb2 // id: %2

package func g1(_ arg: PkgEnumWithPublicCase) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val.publicVar
  }
}

// CHECK: // g1(_:)
// CHECK-NEXT: sil @$s6Client2g1ySi5Utils21PkgEnumWithPublicCaseOF : $@convention(thin) (@in_guaranteed PkgEnumWithPublicCase) -> Int {
// CHECK-NEXT: // %0 "arg"                                       // users: %3, %1
// CHECK-NEXT: bb0(%0 : $*PkgEnumWithPublicCase):
// CHECK-NEXT:   debug_value %0 : $*PkgEnumWithPublicCase, let, name "arg", argno 1, expr op_deref // id: %1
// CHECK-NEXT:   %2 = alloc_stack $PkgEnumWithPublicCase         // users: %29, %9, %7, %4, %3
// CHECK-NEXT:   copy_addr %0 to [init] %2 : $*PkgEnumWithPublicCase // id: %3
// CHECK-NEXT:   switch_enum_addr %2 : $*PkgEnumWithPublicCase, case #PkgEnumWithPublicCase.one!enumelt: bb1, case #PkgEnumWithPublicCase.two!enumelt: bb2 // id: %4

package func g2(_ arg: PkgEnumWithExistentialCase) -> any StringProtocol {
  switch arg { // no-warning
  case .one:
    return "1"
  case .two(let val):
    return val
  }
}

// CHECK: // g2(_:)
// CHECK-NEXT: sil @$s6Client2g2ySy_p5Utils26PkgEnumWithExistentialCaseOF : $@convention(thin) (@in_guaranteed PkgEnumWithExistentialCase) -> @out any StringProtocol {
// CHECK-NEXT: // %0 "$return_value"                             // users: %20, %12
// CHECK-NEXT: // %1 "arg"                                       // users: %4, %2
// CHECK-NEXT: bb0(%0 : $*any StringProtocol, %1 : $*PkgEnumWithExistentialCase):
// CHECK-NEXT:   debug_value %1 : $*PkgEnumWithExistentialCase, let, name "arg", argno 1, expr op_deref // id: %2
// CHECK-NEXT:   %3 = alloc_stack $PkgEnumWithExistentialCase    // users: %23, %16, %14, %5, %4
// CHECK-NEXT:   copy_addr %1 to [init] %3 : $*PkgEnumWithExistentialCase // id: %4
// CHECK-NEXT:   switch_enum_addr %3 : $*PkgEnumWithExistentialCase, case #PkgEnumWithExistentialCase.one!enumelt: bb1, case #PkgEnumWithExistentialCase.two!enumelt: bb2 // id: %5


@inlinable
package func h(_ arg: UfiPkgEnum) -> Int {
  switch arg { // expected-warning {{switch covers known cases, but 'UfiPkgEnum' may have additional unknown values}} {{none}} expected-note {{handle unknown values using "@unknown default"}}
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

// CHECK: // h(_:)
// CHECK-NEXT: sil @$s6Client1hySi5Utils10UfiPkgEnumOF : $@convention(thin) (@in_guaranteed UfiPkgEnum) -> Int {
// CHECK-NEXT: // %0 "arg"                                       // users: %3, %1
// CHECK-NEXT: bb0(%0 : $*UfiPkgEnum):
// CHECK-NEXT:   debug_value %0 : $*UfiPkgEnum, let, name "arg", argno 1, expr op_deref // id: %1
// CHECK-NEXT:   %2 = alloc_stack $UfiPkgEnum                    // users: %21, %10, %8, %5, %4, %3
// CHECK-NEXT:   copy_addr %0 to [init] %2 : $*UfiPkgEnum        // id: %3
// CHECK-NEXT:   %4 = value_metatype $@thick UfiPkgEnum.Type, %2 : $*UfiPkgEnum // user: %24
// CHECK-NEXT:   switch_enum_addr %2 : $*UfiPkgEnum, case #UfiPkgEnum.one!enumelt: bb1, case #UfiPkgEnum.two!enumelt: bb2, default bb3 // id: %5

public func k(_ arg: PublicEnum) -> Int {
  switch arg { // expected-warning {{switch covers known cases, but 'PublicEnum' may have additional unknown values}} {{none}} expected-note {{handle unknown values using "@unknown default"}}
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}
// CHECK: // k(_:)
// CHECK-NEXT: sil @$s6Client1kySi5Utils10PublicEnumOF : $@convention(thin) (@in_guaranteed PublicEnum) -> Int {
// CHECK-NEXT: // %0 "arg"                                       // users: %3, %1
// CHECK-NEXT: bb0(%0 : $*PublicEnum):
// CHECK-NEXT:   debug_value %0 : $*PublicEnum, let, name "arg", argno 1, expr op_deref // id: %1
// CHECK-NEXT:   %2 = alloc_stack $PublicEnum                    // users: %21, %10, %8, %5, %4, %3
// CHECK-NEXT:   copy_addr %0 to [init] %2 : $*PublicEnum        // id: %3
// CHECK-NEXT:   %4 = value_metatype $@thick PublicEnum.Type, %2 : $*PublicEnum // user: %24
// CHECK-NEXT:   switch_enum_addr %2 : $*PublicEnum, case #PublicEnum.one!enumelt: bb1, case #PublicEnum.two!enumelt: bb2, default bb3 // id: %5

public func m(_ arg: FrozenPublicEnum) -> Int {
  switch arg { // no-warning
  case .one:
    return 1
  case .two(let val):
    return 2 + val
  }
}

// CHECK: // m(_:)
// CHECK-NEXT: sil @$s6Client1mySi5Utils16FrozenPublicEnumOF : $@convention(thin) (FrozenPublicEnum) -> Int {
// CHECK-NEXT: // %0 "arg"                                       // users: %2, %1
// CHECK-NEXT: bb0(%0 : $FrozenPublicEnum):
// CHECK-NEXT:   debug_value %0 : $FrozenPublicEnum, let, name "arg", argno 1 // id: %1
// CHECK-NEXT:   switch_enum %0 : $FrozenPublicEnum, case #FrozenPublicEnum.one!enumelt: bb1, case #FrozenPublicEnum.two!enumelt: bb2 // id: %2


