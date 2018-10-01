// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

// CHECK: function_ref @$sSlsE9dropFirsty11SubSequenceQzSiF
_ = [1, 2, 3].dropFirst(1).dropFirst(1)
