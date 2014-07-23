// RUN: %swift %s -emit-sil -Ofast | sil-opt -verify
// XFAIL: *
// XFAIL due to <rdar://problem/17758203> and <rdar://problem/17781140>
var W = [UInt32](count: 16, repeatedValue: 0)
