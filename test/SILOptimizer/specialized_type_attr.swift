// RUN: %target-swift-frontend %s -parse-as-library -module-name M -enable-experimental-feature ExtendedSpecializedAttribute -O -emit-sil -o %t.sil
// RUN: %FileCheck -check-prefix GET %s < %t.sil
// RUN: %FileCheck -check-prefix MODIFY %s < %t.sil
// RUN: %FileCheck -check-prefix NOT-EXPORTED %s < %t.sil

// REQUIRES: swift_feature_ExtendedSpecializedAttribute

// On a type, a non-exported '@specialized' adds the specialization to each of
// its members, except coroutine accessors.

// It doesn't export anything.
// NOT-EXPORTED-NOT: @$s1M{{[^ ]*}}Ts5
// NOT-EXPORTED-NOT: [specialized exported: true{{.*}}@$s1M

@specialized(where T == Int)
public struct Box<T> {
  public var v: T
  public func get() -> T { v }
}

// GET-LABEL: sil {{.*}}@$s1M3BoxV3getxyF :
// GET: metatype $@thick Int.Type
// GET: builtin "cmp_eq_Word"
// GET: } // end sil function '$s1M3BoxV3getxyF'

// MODIFY-LABEL: sil {{.*}}@$s1M3BoxV1vxvM :
// MODIFY-NOT: metatype $@thick Int.Type
// MODIFY: } // end sil function '$s1M3BoxV1vxvM'
