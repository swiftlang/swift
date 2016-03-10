// RUN: %target-swift-frontend %s -emit-sil -Ounchecked | %target-sil-opt
var W = [UInt32](repeating: 0, count: 16)
