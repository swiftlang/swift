// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/Library.swift -parse-as-library -module-name Library -enable-experimental-feature ExtendedSpecializedAttribute -O -validate-tbd-against-ir=none -emit-module -emit-module-path %t/Library.swiftmodule -emit-ir -o %t/Library.ll
// RUN: %FileCheck -check-prefix LIBRARY %s < %t/Library.ll
// RUN: %FileCheck -check-prefix FAST-PATH %s < %t/Library.ll
// RUN: %target-swift-frontend %t/Library.swift -parse-as-library -module-name Library -enable-experimental-feature ExtendedSpecializedAttribute -emit-silgen -o - | %target-sil-opt -enable-experimental-feature ExtendedSpecializedAttribute -eager-specializer -eager-specializer | %FileCheck -check-prefix TWICE %s
// RUN: %target-swift-frontend %t/Client.swift -parse-as-library -module-name Client -I %t -O -emit-ir -o - | %FileCheck -check-prefix CLIENT-O %s --implicit-check-not 'define {{.*}}@"$s7Library3sumyxx_xts17FixedWidthIntegerRzlFSi_'

// REQUIRES: swift_feature_ExtendedSpecializedAttribute

//--- Library.swift

// LIBRARY-DAG: define {{.*}}swiftcc {{i(32|64)}} @"$s7Library3sumyxx_xts17FixedWidthIntegerRzlFSi_Ts5"
@specialized(exported: true, where T == Int)
@inline(never)
public func sum<T: FixedWidthInteger>(_ a: T, _ b: T) -> T { a &+ b &* 3 }

// The generic bodies have a fast path for 'Int'.
// FAST-PATH-LABEL: define {{.*}}@"$s7Library3sumyxx_xts17FixedWidthIntegerRzlF"(
// FAST-PATH: icmp eq ptr %T, @"$sSiN"
// FAST-PATH-LABEL: define {{.*}}@"$s7Library3BoxV3getxyF"(
// FAST-PATH: icmp eq ptr %T, @"$sSiN"
// Running the pass again doesn't add a second one.
// TWICE-LABEL: sil {{.*}}@$s7Library3sumyxx_xts17FixedWidthIntegerRzlF :
// TWICE: builtin "cmp_eq_Word"
// TWICE-NOT: builtin "cmp_eq_Word"
// TWICE: } // end sil function '$s7Library3sumyxx_xts17FixedWidthIntegerRzlF'

// On a type, the members get exported specialized bodies.
// LIBRARY-DAG: define {{.*}}swiftcc {{i(32|64)}} @"$s7Library3BoxV3getxyFSi_Ts5"
@specialized(exported: true, where T == Int)
public struct Box<T> {
  public var v: T
  public init(_ v: T) { self.v = v }
  @inline(never) public func get() -> T { v }
}

//--- Client.swift

import Library

// An optimized client calls the library's specialized body.
// CLIENT-O-LABEL: define {{.*}}@"$s6Client3useyS2iF"
// CLIENT-O: call swiftcc {{i(32|64)}} @"$s7Library3sumyxx_xts17FixedWidthIntegerRzlFSi_Ts5"
public func use(_ x: Int) -> Int { sum(x, x) }

// CLIENT-O-LABEL: define {{.*}}@"$s6Client6useBoxyS2iF"
// CLIENT-O: call swiftcc {{i(32|64)}} @"$s7Library3BoxV3getxyFSi_Ts5"
public func useBox(_ x: Int) -> Int { Box(x).get() }
