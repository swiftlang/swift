// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/A.swift -parse-as-library -O -enable-default-cmo -emit-module -emit-module-path %t/A.swiftmodule
// B's weak bodies aren't part of its interface, so its TBD (Darwin) doesn't list them.
// RUN: %target-swift-frontend %t/B.swift -I %t -module-name B -parse-as-library -enable-experimental-feature ExtendedSpecializedAttribute -O -emit-module -emit-module-path %t/B.swiftmodule %if OS_FAMILY=darwin %{ -validate-tbd-against-ir=all %} %else %{ -validate-tbd-against-ir=none %} -emit-ir -o - | %FileCheck -check-prefix B %s --implicit-check-not '@{{.}}$s1B' --implicit-check-not 'viaInternal{{.*}}Ts5' --implicit-check-not '@{{.}}$s1A3BoxVySiGMf' --implicit-check-not 'plain{{.*}}Si_T' --implicit-check-not 's5UInt8V{{.*}}Ts5'
// RUN: %target-swift-frontend %t/C.swift -I %t -module-name C -parse-as-library -enable-experimental-feature ExtendedSpecializedAttribute -O -emit-ir | %FileCheck -check-prefix C %s --implicit-check-not 'viaInternal{{.*}}Ts5'

// A client that links B through another module's import uses B's bodies too.
// RUN: %target-swift-frontend %t/X.swift -I %t -parse-as-library -O -emit-module -emit-module-path %t/X.swiftmodule
// RUN: %target-swift-frontend %t/Y.swift -I %t -module-name Y -parse-as-library -enable-experimental-feature ExtendedSpecializedAttribute -O -emit-ir | %FileCheck -check-prefix Y %s

// Targets that can't be specialized.
// RUN: not %target-swift-frontend %t/E.swift -I %t -parse-as-library -enable-experimental-feature ExtendedSpecializedAttribute -O -emit-ir -o /dev/null 2>&1 | %FileCheck -check-prefix E %s
// E: error: cannot export a specialization of 'noOptimization', which can't be specialized, e.g. because it's 'dynamic' or '@_optimize(none)'
// E: error: cannot export a specialization of 'dyn', which can't be specialized

// Without cross-module optimization, A's bodies aren't available to B.
// RUN: %target-swift-frontend %t/A.swift -parse-as-library -O -disable-cmo -emit-module -emit-module-path %t/NoCMO/A.swiftmodule
// RUN: not %target-swift-frontend %t/B.swift -I %t/NoCMO -parse-as-library -enable-experimental-feature ExtendedSpecializedAttribute -O -emit-ir -o /dev/null 2>&1 | %FileCheck -check-prefix NOCMO %s
// NOCMO: error: cannot export a specialization of 'sum', whose body isn't available to this module unless it's '@inlinable' or its module is built with cross-module optimization

// REQUIRES: concurrency
// REQUIRES: swift_feature_ExtendedSpecializedAttribute

//--- A.swift

public func sum<T: FixedWidthInteger>(_ a: T, _ b: T) -> T { a &+ b }
@inlinable public func inl<T: FixedWidthInteger>(_ a: T) -> T { a &+ 2 }

public struct Box<T> {
  public var v: T
  public init(_ v: T) { self.v = v }
  public func get() -> T { v }
  // Not serialized: it uses an internal function.
  public func viaInternal() -> T { helper(v) }
}
func helper<T>(_ t: T) -> T { t }

public func asyncSum<T: FixedWidthInteger>(_ a: T, _ b: T) async -> T { a &+ b }

open class K<T> {
  public var v: T
  public init(_ v: T) { self.v = v }
  public convenience init(copy: K) { self.init(copy.v) }
}

public func plain<T>(_ t: T) -> T { t }

@_optimize(none) public func noOptimization<T>(_ t: T) -> T { t }
public dynamic func dyn<T>(_ t: T) -> T { t }

//--- B.swift

import A

// B's bodies are weak, and B has no symbols of its own (implicit-check-not).
// B-DAG: define weak_odr {{(protected |dllexport )?}}swiftcc {{i(32|64)}} @"$s1A3sumyxx_xts17FixedWidthIntegerRzlFSi_Ts5"
// B-DAG: define weak_odr {{(protected |dllexport )?}}swiftcc {{i(32|64)}} @"$s1A3inlyxxs17FixedWidthIntegerRzlFSi_Ts5"
@specialized(exported: true, where T == Int)
public func sum<T: FixedWidthInteger>(_ a: T, _ b: T) -> T

@specialized(exported: true, where T == Int)
public func inl<T: FixedWidthInteger>(_ a: T) -> T

// Also an async function's function pointer, and a class's convenience
// initializer (which has only an allocating entry point).
// B-DAG: @"$s1A8asyncSumyxx_xtYas17FixedWidthIntegerRzlFSi_Ts5Tu" = weak_odr {{(protected |dllexport )?}}global
// B-DAG: define weak_odr {{(protected |dllexport )?}}swiftcc {{.*}}@"$s1A1KC4copyACyxGAE_tcfCSi_Ts5"
@specialized(exported: true, where T == Int)
public func asyncSum<T: FixedWidthInteger>(_ a: T, _ b: T) async -> T

extension K {
  @specialized(exported: true, where T == Int)
  public convenience init(copy: K)
}

// Only members with available bodies (not 'viaInternal'), and no metadata.
// B-DAG: define weak_odr {{(protected |dllexport )?}}swiftcc {{i(32|64)}} @"$s1A3BoxV3getxyFSi_Ts5"
@specialized(exported: true, where T == Int)
extension Box {}

// Without 'exported: true', no effect (checked with '--implicit-check-not').
@specialized(where T == Int)
public func plain<T>(_ t: T) -> T

@specialized(where T == UInt8)
extension Box {}

//--- C.swift

import A
import B

// C calls B's bodies, but not one for 'viaInternal', which B doesn't have.
// C-DAG: declare {{.*}}@"$s1A3sumyxx_xts17FixedWidthIntegerRzlFSi_Ts5"
// C-DAG: declare {{.*}}@"$s1A3BoxV3getxyFSi_Ts5"
public func use(_ x: Int) -> Int {
  sum(x, x) &+ inl(x) &+ Box(x).get() &+ Box(x).viaInternal()
}

//--- X.swift

import B

//--- Y.swift

import A
import X

// Y: call {{.*}}@"$s1A3BoxV3getxyFSi_Ts5"
public func use(_ x: Int) -> Int { Box(x).get() }

//--- E.swift

import A

@specialized(exported: true, where T == Int)
public func noOptimization<T>(_ t: T) -> T

@specialized(exported: true, where T == Int)
public func dyn<T>(_ t: T) -> T
