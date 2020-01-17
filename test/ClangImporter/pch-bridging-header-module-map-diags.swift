// RUN: rm -f %t.*
// REQUIRES: objc_interop

// Build a bridging PCH for involving a module map that contains a warning
// RUN: %target-swift-frontend -F %S/Inputs/ModuleMapWarning -emit-pch %S/Inputs/ModuleMapWarning/bridging-pch.h -pch-output-dir %t/pch 2> %t.stderr
// RUN: %FileCheck %s < %t.stderr

// CHECK: module.private.modulemap:1:33: warning: private submodule 'PrivateWarning.Private' in private module map, expected top-level module

// Check that loading that bridging PCH Does not crash the compiler.
// RUN: %target-swift-frontend -F %S/Inputs/ModuleMapWarning -import-objc-header %S/Inputs/ModuleMapWarning/bridging-pch.h -pch-output-dir %t/pch -typecheck %s


