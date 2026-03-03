// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %s \
// RUN: -Xllvm -sil-print-types -emit-sil -module-name=Lib -package-name Pkg \
// RUN: -package-cmo -allow-non-resilient-access \
// RUN: -O -wmo -enable-library-evolution \
// RUN: -Xllvm -sil-print-types -Xllvm -sil-print-function=s3Lib3fooyS2iF 2>&1 | %FileCheck %s

/// TEST that accessing PkgStruct in the following functions gets inlined after perf inlining pass.
public func bar(_ arg: Int) -> Int {
  let p = PkgStruct(1, 2)
  return arg > 0 ? p.field1 : p.field2
}

package func foo(_ arg: Int) -> Int {
  let p = PkgStruct(1, 2)
  return arg > 0 ? p.field1 : p.field2
}

package struct PkgStruct {
  package let field1: Int
  package let field2: Int
  package init(_ arg1: Int, _ arg2: Int) {
    field1 = arg1
    field2 = arg2
  }
}

/// BEFORE perf inlining pass.
// CHECK: sil package [ossa] @$s3Lib3fooyS2iF : $@convention(thin) (Int) -> Int {
// CHECK:  [[PKG_STACK:%.*]] = alloc_stack $PkgStruct
// CHECK:  [[FUNC_REF:%.*]] = function_ref @$s3Lib9PkgStructVyACSi_SitcfC : $@convention(method) (Int, Int, @thin PkgStruct.Type) -> @out PkgStruct
// CHECK:   apply [[FUNC_REF]]
// CHECK:   [[F1:%.*]] = struct_element_addr [[PKG_STACK]] : $*PkgStruct, #PkgStruct.field1
// CHECK:   load [trivial] [[F1]] : $*Int
// CHECK:   [[F2:%.*]] = struct_element_addr [[PKG_STACK]] : $*PkgStruct, #PkgStruct.field2
// CHECK:   load [trivial] [[F2]] : $*Int
 
/// AFTER perf inlining pass; body of `@$s3Lib9PkgStructVyACSi_SitcfC` gets inlined.
// CHECK: *** SIL function after {{.*}} EarlyPerfInliner (early-inline)
// CHECK: sil package [ossa] @$s3Lib3fooyS2iF : $@convention(thin) (Int) -> Int {
// CHECK:   [[PKG_ALLOC:%.*]] = alloc_stack $PkgStruct
// CHECK:   [[FIELD1_IVAR:%.*]] = struct_element_addr [[PKG_ALLOC]] : $*PkgStruct, #PkgStruct.field1
// CHECK:   store {{.*}} to [trivial] [[FIELD1_IVAR]] : $*Int
// CHECK:   [[FIELD2_IVAR:%.*]] = struct_element_addr [[PKG_ALLOC]] : $*PkgStruct, #PkgStruct.field2
// CHECK:   store {{.*}} to [trivial] [[FIELD2_IVAR]] : $*Int
// CHECK:   [[FIELD1:%.*]] = struct_element_addr [[PKG_ALLOC]] : $*PkgStruct, #PkgStruct.field1
// CHECK:   load [trivial] [[FIELD1]] : $*Int
// CHECK:   [[FIELD2:%.*]] = struct_element_addr [[PKG_ALLOC]] : $*PkgStruct, #PkgStruct.field2
// CHECK:   load [trivial] [[FIELD2]] : $*Int
