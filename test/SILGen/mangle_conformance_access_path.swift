// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/mangle_conformance_access_path_helper.swift -emit-module-path %t/mangle_conformance_access_path_helper.swiftmodule
// RUN: %target-swift-frontend -emit-silgen %s -I %t -requirement-machine=verify | %FileCheck %s

import mangle_conformance_access_path_helper

struct GG<T : P> {}

// This is a retroactive conformance.
extension G : P where T : P {}

// The mangling of GG<G<_>> will contain a conformance access path
// for (Y.U.U : P). This path is (Y : R)(Self.U.U : P). The 'Self.U.U'
// uses the short mangling if 'Self.U' only conforms to a single
// protocol. However, this check was being performed in the original
// generic signature <X, Y where Y : R>, and not the generic signature
// for the protocol R, which is <Self where Self : R>.

// CHECK-LABEL: sil hidden [ossa] @$s30mangle_conformance_access_path3fooyyAA2GGVy0a1_b1_c1_D7_helper1GVy1U_AHQY_GAjE1PAAq_AE1RHD1_AH_AHQZAeKHA2__HCg_G_xq_tAeLR_r0_lF : $@convention(thin) <X, Y where Y : R> (GG<G<Y.U.U>>, @in_guaranteed X, @in_guaranteed Y) -> ()
func foo<X, Y : R>(_: GG<G<Y.U.U>>, _: X, _: Y) {}
