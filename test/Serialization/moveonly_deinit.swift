// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-move-only -g -emit-module  -module-name OtherModule %S/Inputs/moveonly_deinit.swift -emit-module-path %t/OtherModule.swiftmodule
// RUN: %target-swift-frontend -enable-experimental-move-only -g -I %t %s -emit-silgen

// Make sure we can deserialize deinits of both enums and structs.

import OtherModule

let s = MoveOnlyStruct(desc: 5)
let e = MoveOnlyEnum.lhs(5)
