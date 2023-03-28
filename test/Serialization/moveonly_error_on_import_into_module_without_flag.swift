// RUN: %empty-directory(%t)
// TODO: re-enable the simplification passes once rdar://104875010 is fixed
// RUN: %target-swift-frontend -Xllvm -sil-disable-pass=simplification -emit-module -o %t/Library.swiftmodule -module-name Library %S/Inputs/moveonly_deinit.swift -enable-experimental-feature MoveOnlyEnumDeinits

// >>> make sure borrow scopes are required when using a move-only type from another module
// RUN: not %target-swift-frontend -DUSE_MOVEONLY_TYPE -typecheck -enable-experimental-feature MoveOnlyEnumDeinits -enable-lexical-borrow-scopes=false -I %t %s 2>&1 | %FileCheck %s --check-prefix CHECK-ERROR
// RUN: %target-swift-frontend -DUSE_MOVEONLY_TYPE -typecheck -I %t %s 2>&1 | %FileCheck %s --allow-empty

// >>> try turning off lexical borrow scopes with no move-only types; shouldn't be an error.
//     we have to pipe into FileCheck because -verify ignores errors from other files.
// RUN: %target-swift-frontend -enable-experimental-feature MoveOnlyEnumDeinits -enable-lexical-borrow-scopes=false -typecheck -I %t %s 2>&1 | %FileCheck %s --allow-empty

// This test makes sure that if we import a move only type
// and do not have lexical borrow scopes enabled, we emit an error.

import Library

// CHECK-ERROR: Library.MoveOnlyStruct{{.*}} error: noncopyable types require lexical borrow scopes (add -enable-lexical-borrow-scopes=true)

// CHECK-NOT: error

#if USE_MOVEONLY_TYPE
func f(_ k: borrowing MoveOnlyStruct) {}
#endif

func g(_ k: NormalStruct) {}
