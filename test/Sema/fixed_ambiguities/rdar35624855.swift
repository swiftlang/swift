// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

extension Collection {
  func foo() {
    // CHECK: function_ref @$sSlsE9dropFirsty11SubSequenceQzSiF
    _ = zip(indices, indices.dropFirst(3))
  }
}
