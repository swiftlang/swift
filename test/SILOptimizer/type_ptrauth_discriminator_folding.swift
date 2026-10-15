// RUN: %target-swift-frontend -parse-stdlib -module-name main -emit-sil -emit-sorted-sil -Onone %s | %FileCheck %s --check-prefixes=CHECK,ONONE
// RUN: %target-swift-frontend -parse-stdlib -module-name main -emit-sil -emit-sorted-sil -O %s | %FileCheck %s --check-prefixes=CHECK,OPT

// `Builtin.typePtrAuthDiscriminator` is folded where it appears, after
// mandatory inlining, so that it hashes the function type as values of that
// type are actually passed there. In a generic context `T` and `U` are passed
// indirectly; specializing the function must not re-lower the type with the
// concrete types' direct conventions, which would hash differently than the
// function values the discriminator is used to sign and authenticate.
//
// The declarations are in the order of their mangled names, which is the order
// `-emit-sorted-sil` prints them in.

import Swift

public func callsGeneric() -> Builtin.Int64 {
  return generic(Int.self, Int.self)
}

// A bare type parameter doesn't determine how its values are passed, so the
// builtin is left alone until it's inlined somewhere that does.
// ONONE-LABEL: sil [noinline] {{.*}}@$s4main4bareyBi64_xmlF :
// ONONE: builtin "typePtrAuthDiscriminator"<T>
// ONONE: } // end sil function '$s4main4bareyBi64_xmlF'
@inline(never)
public func bare<T>(_ type: T.Type) -> Builtin.Int64 {
  return Builtin.typePtrAuthDiscriminator(type)
}

// This is the shape of `ComputedAccessorsPtr.Getter` in KeyPath.swift.
typealias Getter<T, U> = @convention(thin) (T, UnsafeRawPointer, Int) -> U

// CHECK-LABEL: sil [noinline] {{.*}}@$s4main7genericyBi64_xm_q_mtr0_lF :
// CHECK-NOT: builtin "typePtrAuthDiscriminator"
// CHECK: integer_literal $Builtin.Int64, [[INDIRECT:[0-9]+]]
// CHECK-NOT: builtin "typePtrAuthDiscriminator"
// CHECK: } // end sil function '$s4main7genericyBi64_xm_q_mtr0_lF'
@inline(never)
public func generic<T, U>(_: T.Type, _: U.Type) -> Builtin.Int64 {
  return Builtin.typePtrAuthDiscriminator(Getter<T, U>.self)
}

// Specializing `generic` keeps its discriminator.
// OPT-LABEL: sil shared {{.*}}@$s4main7genericyBi64_xm_q_mtr0_lFSi_SiT{{.*}}g5 :
// OPT-NOT: builtin "typePtrAuthDiscriminator"
// OPT: integer_literal $Builtin.Int64, [[INDIRECT]]{{$| }}
// OPT: } // end sil function '$s4main7genericyBi64_xm_q_mtr0_lFSi_SiT{{.*}}g5'

// C function pointers use Clang's type discriminators, which IRGen computes.
// CHECK-LABEL: sil [noinline] {{.*}}@$s4main8cPointerBi64_yF :
// CHECK: builtin "typePtrAuthDiscriminator"<@convention(c) (Int) -> Int>
// CHECK: } // end sil function '$s4main8cPointerBi64_yF'
@inline(never)
public func cPointer() -> Builtin.Int64 {
  return Builtin.typePtrAuthDiscriminator((@convention(c) (Int) -> Int).self)
}

// The same function type as in `generic`, written with concrete types, is
// lowered with direct conventions, so it has a different discriminator.
// CHECK-LABEL: sil [noinline] {{.*}}@$s4main8concreteBi64_yF :
// CHECK-NOT: integer_literal $Builtin.Int64, [[INDIRECT]]{{$| }}
// CHECK: integer_literal $Builtin.Int64,
// CHECK-NOT: integer_literal $Builtin.Int64, [[INDIRECT]]{{$| }}
// CHECK: } // end sil function '$s4main8concreteBi64_yF'
@inline(never)
public func concrete() -> Builtin.Int64 {
  return Builtin.typePtrAuthDiscriminator(Getter<Int, Int>.self)
}
