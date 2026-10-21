// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/Library.swift -parse-as-library -module-name Library -enable-experimental-feature Embedded -enable-experimental-feature ExtendedSpecializedAttribute -O -emit-ir -o %t/Library-O.ll
// RUN: %FileCheck -check-prefix LIBRARY %s < %t/Library-O.ll
// RUN: %FileCheck -check-prefix LIBRARY-OLD %s < %t/Library-O.ll
// RUN: %target-swift-frontend %t/Library.swift -parse-as-library -module-name Library -enable-experimental-feature Embedded -enable-experimental-feature ExtendedSpecializedAttribute -Onone -emit-ir -o - | %FileCheck -check-prefix LIBRARY %s

// SIL: the attribute spelling survives a textual round trip.
// RUN: %target-swift-frontend %t/Library.swift -parse-as-library -module-name Library -enable-experimental-feature Embedded -enable-experimental-feature ExtendedSpecializedAttribute -emit-silgen -o %t/Library.silgen
// RUN: %FileCheck -check-prefix LIBRARY-SILGEN %s < %t/Library.silgen
// RUN: %target-sil-opt %t/Library.silgen -module-name Library -enable-experimental-feature Embedded -enable-experimental-feature ExtendedSpecializedAttribute | %FileCheck -check-prefix LIBRARY-SILGEN %s

// RUN: %target-swift-frontend %t/Library.swift -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature ExtendedSpecializedAttribute -O -emit-module -emit-module-path %t/Modules/Library.swiftmodule
// RUN: %target-swift-frontend %t/Application.swift -I %t/Modules -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature ExtendedSpecializedAttribute -O -emit-ir -o - | %FileCheck -check-prefix APPLICATION %s --implicit-check-not 'define {{.*}}@"$e7Library3sumyxx_xts17FixedWidthIntegerRzlFSi_'
// RUN: %target-swift-frontend %t/Application.swift -I %t/Modules -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature ExtendedSpecializedAttribute -Onone -emit-ir -o - | %FileCheck -check-prefix APPLICATION %s --implicit-check-not 'define {{.*}}@"$e7Library3sumyxx_xts17FixedWidthIntegerRzlFSi_'

// Without the flag, a client builds its own specialized body.
// RUN: %target-swift-frontend %t/Application.swift -I %t/Modules -parse-as-library -enable-experimental-feature Embedded -O -emit-ir -o - | %FileCheck -check-prefix NOFLAG %s

// A module built with the 'implementation' code generation model can't own
// specialized bodies.
// RUN: not %target-swift-frontend %t/Library.swift -parse-as-library -module-name Library -enable-experimental-feature Embedded -enable-experimental-feature ExtendedSpecializedAttribute -enable-experimental-feature CodeGenerationModel=implementation -typecheck 2>&1 | %FileCheck -check-prefix IMPLEMENTATION %s
// IMPLEMENTATION: error: '@specialized(exported: true)' cannot be combined with the 'implementation' code generation model

// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_ExtendedSpecializedAttribute

//--- Library.swift

// The library always emits the promised specialized body, with a strong
// definition.
// LIBRARY-DAG: define {{(protected |dllexport )?}}swiftcc {{i(32|64)}} @"$e7Library3sumyxx_xts17FixedWidthIntegerRzlFSi_Ts5"
// LIBRARY-SILGEN-DAG: sil {{.*}}[specialized exported: true, kind: full, where T == Int] {{.*}}@$e7Library3sumyxx_xts17FixedWidthIntegerRzlF :
@specialized(exported: true, where T == Int)
@inline(never)
public func sum<T: FixedWidthInteger>(_ a: T, _ b: T) -> T { a &+ b &* 3 }

// '@_specialize' keeps today's behavior: no specialized body at -O, and the old
// SIL spelling.
// LIBRARY-OLD-NOT: @"$e7Library3oldyxx_xts17FixedWidthIntegerRzlFSi_Ts5"
// LIBRARY-SILGEN-DAG: sil {{.*}}[_specialize exported: true, kind: full, where T == Int] {{.*}}@$e7Library3oldyxx_xts17FixedWidthIntegerRzlF :
@_specialize(exported: true, where T == Int)
@inline(never)
public func old<T: FixedWidthInteger>(_ a: T, _ b: T) -> T { a &+ b &* 5 }

//--- Application.swift

import Library

// The client calls the library's specialized body and never defines its own
// (checked with '--implicit-check-not').
// APPLICATION-LABEL: define {{.*}}@"$e11Application3useyS2iF"
// APPLICATION: call swiftcc {{i(32|64)}} @"$e7Library3sumyxx_xts17FixedWidthIntegerRzlFSi_Ts5"({{i(32|64)}} %0, {{i(32|64)}} %0)
// NOFLAG: define linkonce_odr hidden {{.*}}@"$e7Library3sumyxx_xts17FixedWidthIntegerRzlFSi_Ts5"
public func use(_ x: Int) -> Int { sum(x, x) }
