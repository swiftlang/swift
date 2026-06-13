// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name attrs -enable-experimental-feature RawLayout
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name attrs
// RUN: %FileCheck %s --input-file %t.swiftinterface

// REQUIRES: swift_feature_RawLayout

// CHECK: @_rawLayout(size: 5, alignment: 4) public struct A_ExplicitSizeAlign
@_rawLayout(size: 5, alignment: 4)
public struct A_ExplicitSizeAlign: ~Copyable {}

// CHECK: @_rawLayout(like: T) public struct B_Cell
@_rawLayout(like: T)
public struct B_Cell<T>: ~Copyable {}

// CHECK: @_rawLayout(like: T, movesAsLike) public struct B2_CellMovesAsLike
@_rawLayout(like: T, movesAsLike)
public struct B2_CellMovesAsLike<T>: ~Copyable {}

// CHECK: @_rawLayout(likeArrayOf: T, count: 8) public struct C_SmallVector
@_rawLayout(likeArrayOf: T, count: 8)
public struct C_SmallVector<T>: ~Copyable {}

// CHECK: @_rawLayout(likeArrayOf: T, count: 8, movesAsLike) public struct C2_SmallVectorMovesAsLike
@_rawLayout(likeArrayOf: T, count: 8, movesAsLike)
public struct C2_SmallVectorMovesAsLike<T>: ~Copyable {}
