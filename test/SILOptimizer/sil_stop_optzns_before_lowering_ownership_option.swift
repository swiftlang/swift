// Test that makes sure the pass pipeline respects the frontend option sil-stop-optzns-before-lowering-ownership
//
// RUN: %target-swift-frontend -sil-stop-optzns-before-lowering-ownership -Osize -emit-sil %s | %FileCheck %s
// RUN: %target-swift-frontend -sil-stop-optzns-before-lowering-ownership -O -emit-sil %s | %FileCheck %s

public class Klass {}

// CHECK-LABEL: sil [ossa] @$s48sil_stop_optzns_before_lowering_ownership_option7theFuncAA5KlassCyF : $@convention(thin) () -> @owned Klass {
public func theFunc() -> Klass { Klass() }
