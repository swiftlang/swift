// RUN: %swift %s -emit-sil -Ounchecked | sil-opt -verify
var W = [UInt32](count: 16, repeatedValue: 0)
