// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module %S/Inputs/autolinking_public.swift -emit-module-path %t/autolinking_public.swiftmodule -module-link-name autolinking_public -swift-version 4
// RUN: %target-swift-frontend -emit-module %S/Inputs/autolinking_other.swift -emit-module-path %t/autolinking_other.swiftmodule -module-link-name autolinking_other -swift-version 4
// RUN: %target-swift-frontend -emit-module %S/Inputs/autolinking_private.swift -emit-module-path %t/autolinking_private.swiftmodule -module-link-name autolinking_private -I %t -swift-version 4
// RUN: %target-swift-frontend -emit-module %S/Inputs/autolinking_other2.swift -emit-module-path %t/autolinking_other2.swiftmodule -module-link-name autolinking_other2 -swift-version 4
// RUN: %target-swift-frontend -emit-module %S/Inputs/autolinking_indirect.swift -emit-module-path %t/autolinking_indirect.swiftmodule -module-link-name autolinking_indirect -I %t -swift-version 4

// RUN: %target-swift-frontend -emit-module %S/Inputs/autolinking_module_inferred.swift -emit-module-path %t/autolinking_module_inferred.swiftmodule -module-link-name autolinking_module_inferred -I %t -swift-version 4
// RUN: %target-swift-frontend -emit-ir %s -I %t -swift-version 4 | %FileCheck %s

// Linux uses a different autolinking mechanism, based on
// swift-autolink-extract. This file tests the Darwin mechanism.
// UNSUPPORTED: OS=linux-gnu
// UNSUPPORTED: OS=linux-gnueabihf
// UNSUPPORTED: OS=freebsd
// UNSUPPORTED: OS=linux-androideabi

import autolinking_module_inferred

bfunc()

// CHECK: !llvm.linker.options = !{[[MODULE:![0-9]+]], [[PUBLIC:![0-9]+]], [[SWIFTONONESUPPORT:![0-9]+]], [[SWIFTCORE:![0-9]+]], [[PRIVATE:![0-9]+]], [[OTHER:![0-9]+]], [[INDIRECT:![0-9]+]], [[OTHER2:![0-9]+]], [[OBJC:![0-9]+]]}

// CHECK-DAG: [[SWIFTCORE]] = !{!"-lswiftCore"}
// CHECK-DAG: [[SWIFTONONESUPPORT]] = !{!"-lswiftSwiftOnoneSupport"}
// CHECK-DAG: [[MODULE]] = !{!"-lautolinking_module_inferred"}
// CHECK-DAG: [[PUBLIC]] = !{!"-lautolinking_public"}
// CHECK-DAG: [[OTHER]] = !{!"-lautolinking_other"}
// CHECK-DAG: [[OTHER2]] = !{!"-lautolinking_other2"}
// CHECK-DAG: [[OBJC]] = !{!"-lobjc"}

// We don't actually care about these two. As long as we autolink the libraries
// that get used, we're okay.
// CHECK-DAG: [[PRIVATE]] = !{!"-lautolinking_private"}
// CHECK-DAG: [[INDIRECT]] = !{!"-lautolinking_indirect"}
