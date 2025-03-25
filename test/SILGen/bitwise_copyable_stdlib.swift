// R N: %target-swift-frontend  -enable-experimental-feature BuiltinModule -parse-stdlib -module-name Swift -DEMPTY -emit-sil -verify %s

// RUN: %target-swift-frontend                               \
// RUN:     -emit-sil                                        \
// RUN:     %s                                               \
// RUN:     -parse-stdlib                                    \
// RUN:     -module-name Swift                               \
// RUN:     -disable-availability-checking                   \
// RUN:     -enable-experimental-feature BuiltinModule       \
// RUN:     -enable-experimental-feature LifetimeDependence  \
// RUN:     -enable-builtin-module

// REQUIRES: swift_feature_BuiltinModule
// REQUIRES: swift_feature_LifetimeDependence

// Force verification of TypeLowering's isTrivial.

import Builtin

@_marker public protocol Copyable: ~Escapable {}
@_marker public protocol Escapable: ~Copyable {}
@_marker public protocol BitwiseCopyable : ~Escapable {}

struct Storage : ~Escapable, BitwiseCopyable {}


func take<T : BitwiseCopyable & ~Escapable>(_ t: T) {}

func passStorage(_ s: Storage) { take(s) }
