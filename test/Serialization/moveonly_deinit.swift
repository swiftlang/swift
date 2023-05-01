// RUN: %empty-directory(%t)
// TODO: re-enable the simplification passes once rdar://104875010 is fixed
// RUN: %target-swift-frontend -enable-experimental-feature MoveOnlyEnumDeinits -Xllvm -sil-disable-pass=simplification -g -emit-module  -module-name OtherModule %S/Inputs/moveonly_deinit.swift -emit-module-path %t/OtherModule.swiftmodule
// RUN: %target-swift-frontend -enable-experimental-feature MoveOnlyEnumDeinits -Xllvm -sil-disable-pass=simplification -g -I %t %s -emit-silgen

// Make sure we can deserialize deinits of both enums and structs.

import OtherModule

let s = MoveOnlyStruct(desc: 5)
let e = MoveOnlyEnum.lhs(5)
