// RUN: %target-swift-frontend %s -emit-sil -Ounchecked | %target-sil-opt
var W = [UInt32](count: 16, repeatedValue: 0)
