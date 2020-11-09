// '@_private import' is intended to be used to compile "thunks" which contain
// modified versions of declarations. For this to work properly, name lookup
// needs to favor declarations in the private import over other imports. This
// emulates the increased visibility of same-module declarations in the original
// file, preserving source compatibility as much as possible.
//
// In this test:
//
// * Module Host contains this file and Host.swift. Note that this file compiles
//   successfully with COMPILING_THUNK undefined.
// * This file is also built as a thunk with COMPILING_THUNK defined to "edit"
//   it.
// * We also build three other modules called Most, Ghost, and Toast. Each of
//   these is imported by both Host and the thunk.
//
// There are various same-name types scattered throughout these modules. If
// name lookup selects the right ones for each of the test cases, the compiler
// will generate the expected diagnostics and the test will pass. If it selects
// incorrect types, deprecation warnings will be emitted and the test will fail.

// We'll put our modules here.
// RUN: %empty-directory(%t)

// Build some libraries to work with.
// RUN: %target-swift-frontend -emit-module -parse-as-library -module-name Most -emit-module-path %t/Most.swiftmodule -primary-file %S/Inputs/private_import/Most.swift
// RUN: %target-swift-frontend -emit-module -parse-as-library -module-name Ghost -emit-module-path %t/Ghost.swiftmodule -primary-file %S/Inputs/private_import/Ghost.swift
// RUN: %target-swift-frontend -emit-module -enable-private-imports -parse-as-library -module-name Toast -emit-module-path %t/Toast.swiftmodule -primary-file %S/Inputs/private_import/Toast.swift

// Build the host module that the thunk is pretending to be part of.
// RUN: %target-swift-frontend -emit-module -module-name Host -I %t -emit-module-path %t/Host.swiftmodule -enable-private-imports %s %S/Inputs/private_import/Host.swift

// Build a thunk for the host module.
// RUN: %target-typecheck-verify-swift -parse-as-library -I %t -DCOMPILING_THUNK

#if COMPILING_THUNK
@_private(sourceFile: "private_import.swift") import Host
#endif

import Most
import Ghost

@_private(sourceFile: "Toast.swift") import Toast

//
// Types with varying definitions
//

struct Hunk {}                  // Present in both Host and thunk

#if COMPILING_THUNK

struct Thunk {}                 // Only present in thunk
struct Bunk {}                  // Not deprecated in thunk

#else

struct Shrunk {}                // Only present in Host
@available(*, deprecated, message: "got Host version")
struct Bunk {}                  // Only deprecated in Host

#endif

//
// Test cases
//

#if COMPILING_THUNK
func thunkCanUseTypesOnlyInThunk(_: Thunk) {}
#endif

func thunkCanRedeclareTypes(_: Hunk) {}
func thunkCanUseTypesOnlyInHost(_: Shrunk, _: Post) {}
func thunkTypesShadowHostTypes(_: Bunk) {}

func hostTypesShadowMostTypes(_: Boast) {}
// no-error@-1; previously, this would give "'Boast' is ambiguous for type lookup in this context"

#if COMPILING_THUNK
func ambiguousWithTwoNonPrivateImports(_: Coast) {}
// expected-error@-1{{'Coast' is ambiguous for type lookup in this context}}
#endif

// The intended clients of '@_private import' should not actually use it twice
// in a single file, so this behavior is more accidental than anything.
func ambiguousWithTwoPrivateImports(_: Roast) {}
// expected-error@-1{{'Roast' is ambiguous for type lookup in this context}}
