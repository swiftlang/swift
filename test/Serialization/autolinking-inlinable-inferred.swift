// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module %S/Inputs/autolinking_public.swift -emit-module-path %t/autolinking_public.swiftmodule -module-link-name autolinking_public -I %t -swift-version 4
// RUN: %target-swift-frontend -emit-module %S/Inputs/autolinking_other3.swift -emit-module-path %t/autolinking_other3.swiftmodule -module-link-name autolinking_other3 -swift-version 4
// RUN: %target-swift-frontend -emit-module %S/Inputs/autolinking_other2.swift -emit-module-path %t/autolinking_other2.swiftmodule -module-link-name autolinking_other2 -I %t -swift-version 4
// RUN: %target-swift-frontend -emit-module %S/Inputs/autolinking_other.swift -emit-module-path %t/autolinking_other.swiftmodule -module-link-name autolinking_other -I %t -swift-version 4
// RUN: %target-swift-frontend -emit-module %S/Inputs/autolinking_private.swift -emit-module-path %t/autolinking_private.swiftmodule -module-link-name autolinking_private -I %t -swift-version 4
// RUN: %target-swift-frontend -emit-module %S/Inputs/autolinking_module_inferred.swift -emit-module-path %t/autolinking_module_inferred.swiftmodule -module-link-name autolinking_module_inferred -I %t -swift-version 4
// RUN: %target-swift-frontend -emit-ir %s -I %t -swift-version 4 | %FileCheck %s

// This test is identical to autolinking-inlinable, except we also rely on the
// '@_usableFromInline' attribute getting inferred correctly on the import.

// Linux uses a different autolinking mechanism, based on
// swift-autolink-extract. This file tests the Darwin mechanism.
// UNSUPPORTED: OS=linux-gnu
// UNSUPPORTED: OS=linux-gnueabihf
// UNSUPPORTED: OS=freebsd
// UNSUPPORTED: OS=linux-androideabi

import autolinking_module_inferred

bfunc()

// Note: we don't autolink autolinking_private even though autolinking_module imports it also.

// CHECK: !llvm.linker.options = !{[[SWIFTCORE:![0-9]+]], [[SWIFTONONESUPPORT:![0-9]+]], [[MODULE:![0-9]+]], [[PUBLIC:![0-9]+]], [[OTHER3:![0-9]+]], [[OTHER:![0-9]+]], [[OBJC:![0-9]+]]}

// CHECK: [[SWIFTCORE]] = !{!"-lswiftCore"}
// CHECK: [[SWIFTONONESUPPORT]] = !{!"-lswiftSwiftOnoneSupport"}
// CHECK: [[MODULE]] = !{!"-lautolinking_module_inferred"}
// CHECK: [[PUBLIC]] = !{!"-lautolinking_public"}
// CHECK: [[OTHER3]] = !{!"-lautolinking_other3"}
// CHECK: [[OTHER]] = !{!"-lautolinking_other"}
// CHECK: [[OBJC]] = !{!"-lobjc"}
