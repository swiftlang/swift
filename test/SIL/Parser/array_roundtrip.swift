// RUN: %swift %s -emit-sil -Ofast | sil-opt -verify

var W = [UInt32](count: 16, repeatedValue: 0)
