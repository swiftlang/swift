// RUN: %target-swift-frontend %s -emit-sil -Ounchecked | %target-sil-opt -assume-parsing-unqualified-ownership-sil
var W = [UInt32](repeating: 0, count: 16)
