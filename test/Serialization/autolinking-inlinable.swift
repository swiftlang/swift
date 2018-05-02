// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module %S/Inputs/autolinking_public.swift -emit-module-path %t/autolinking_public.swiftmodule -module-link-name autolinking_public -swift-version 4
// RUN: %target-swift-frontend -emit-module %S/Inputs/autolinking_other.swift -emit-module-path %t/autolinking_other.swiftmodule -module-link-name autolinking_other -swift-version 4
// RUN: %target-swift-frontend -emit-module %S/Inputs/autolinking_private.swift -emit-module-path %t/autolinking_private.swiftmodule -module-link-name autolinking_private -I %t -swift-version 4
// RUN: %target-swift-frontend -emit-module %S/Inputs/autolinking_module.swift -emit-module-path %t/autolinking_module.swiftmodule -module-link-name autolinking_module -I %t -swift-version 4
// RUN: %target-swift-frontend -emit-ir %s -I %t -swift-version 4 | %FileCheck %s

// Linux uses a different autolinking mechanism, based on
// swift-autolink-extract. This file tests the Darwin mechanism.
// UNSUPPORTED: OS=linux-gnu
// UNSUPPORTED: OS=linux-gnueabihf
// UNSUPPORTED: OS=freebsd
// UNSUPPORTED: OS=linux-androideabi

import autolinking_module

bfunc()

// Note: we don't autolink autolinking_private even though autolinking_module imports it also.

// CHECK: !llvm.linker.options = !{[[SWIFTCORE:![0-9]+]], [[SWIFTONONESUPPORT:![0-9]+]], [[MODULE:![0-9]+]], [[PUBLIC:![0-9]+]], [[OTHER:![0-9]+]], [[OBJC:![0-9]+]]}

// CHECK: [[SWIFTCORE]] = !{!"-lswiftCore"}
// CHECK: [[SWIFTONONESUPPORT]] = !{!"-lswiftSwiftOnoneSupport"}
// CHECK: [[MODULE]] = !{!"-lautolinking_module"}
// CHECK: [[PUBLIC]] = !{!"-lautolinking_public"}
// CHECK: [[OTHER]] = !{!"-lautolinking_other"}
// CHECK: [[OBJC]] = !{!"-lobjc"}
