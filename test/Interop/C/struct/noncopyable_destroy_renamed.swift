// A swift_name renaming a C function to a type's `deinit` used to crash the
// importer. It should ignore the invalid name and import under the original C
// name, which the `destroy:` attribute then resolves to.

// RUN: %target-swift-frontend -emit-sil -I %S%{fs-sep}Inputs %s -verify -verify-additional-file %S%{fs-sep}Inputs%{fs-sep}noncopyable-destroy-renamed.h | %FileCheck %s

import NoncopyableDestroyRenamed

func use(_ x: borrowing RenamedDestroyType) { }

// The synthesized deinit calls the destroy function, imported under its
// original C name.
// CHECK-LABEL: sil_moveonlydeinit RenamedDestroyType {
// CHECK: @$sSo18RenamedDestroyTypeVfD
