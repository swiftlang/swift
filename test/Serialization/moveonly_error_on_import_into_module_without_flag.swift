// RUN: %empty-directory(%t)
// TODO: re-enable the simplification passes once rdar://104875010 is fixed
// RUN: %target-swift-frontend -Xllvm -sil-disable-pass=simplification -emit-module -o %t/Library.swiftmodule -module-name Library %S/Inputs/moveonly_deinit.swift -enable-experimental-move-only
// RUN: not %target-swift-frontend -DUSE_MOVEONLY_TYPE -I %t %s -emit-sil -o /dev/null 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -I %t %s -emit-sil -verify -o /dev/null

// This test makes sure that if we import a move only type and do not set the
// experimental move only flag, we get a nice error.

import Library

// CHECK: error: Can not import module 'Library' that uses move only features when experimental move only is disabled! Pass the frontend flag -enable-experimental-move-only to swift to enable the usage of this language feature

#if USE_MOVEONLY_TYPE
func f(_ k: MoveOnlyStruct) {}
#endif

func g(_ k: NormalStruct) {}
