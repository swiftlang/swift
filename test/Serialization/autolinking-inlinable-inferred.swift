// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module %S/Inputs/autolinking_public.swift -emit-module-path %t/autolinking_public.swiftmodule -module-link-name autolinking_public -swift-version 4
// RUN: %target-swift-frontend -emit-module %S/Inputs/autolinking_other.swift -emit-module-path %t/autolinking_other.swiftmodule -module-link-name autolinking_other -swift-version 4
// RUN: %target-swift-frontend -emit-module %S/Inputs/autolinking_private.swift -emit-module-path %t/autolinking_private.swiftmodule -module-link-name autolinking_private -I %t -swift-version 4
// RUN: %target-swift-frontend -emit-module %S/Inputs/autolinking_other2.swift -emit-module-path %t/autolinking_other2.swiftmodule -module-link-name autolinking_other2 -swift-version 4
// RUN: %target-swift-frontend -emit-module %S/Inputs/autolinking_indirect.swift -emit-module-path %t/autolinking_indirect.swiftmodule -module-link-name autolinking_indirect -I %t -swift-version 4
// RUN: %target-swift-frontend -emit-module %S/Inputs/autolinking_implementation_only.swift -emit-module-path %t/autolinking_implementation_only.swiftmodule -module-link-name autolinking_implementation_only_BAD -I %t -swift-version 4

// RUN: %target-swift-frontend -emit-module %S/Inputs/autolinking_module_inferred.swift -emit-module-path %t/autolinking_module_inferred.swiftmodule -module-link-name autolinking_module_inferred -I %t -swift-version 4

// RUN: %target-swift-frontend -emit-ir %s -I %t -swift-version 4 -enable-objc-interop -D NORMAL_IMPORT | %FileCheck -check-prefix CHECK -check-prefix CHECK-NORMAL -check-prefix CHECK-NORMAL-%target-os -implicit-check-not BAD %s
// RUN: %target-swift-frontend -emit-ir %s -I %t -swift-version 4 -enable-objc-interop -D IMPLEMENTATION_ONLY_IMPORT | %FileCheck -check-prefix CHECK -check-prefix CHECK-IMPL_ONLY -check-prefix CHECK-IMPL_ONLY-%target-os -implicit-check-not BAD %s

// RUN: %target-swift-frontend -emit-ir %s -I %t -swift-version 4 -enable-objc-interop -D NORMAL_AND_IMPLEMENTATION_ONLY | %FileCheck -check-prefix CHECK -check-prefix CHECK-NORMAL -check-prefix CHECK-NORMAL-%target-os -implicit-check-not BAD %s
// RUN: %target-swift-frontend -emit-ir %s -I %t -swift-version 4 -enable-objc-interop -D IMPLEMENTATION_ONLY_AND_NORMAL | %FileCheck -check-prefix CHECK -check-prefix CHECK-NORMAL -check-prefix CHECK-NORMAL-%target-os -implicit-check-not BAD %s
// RUN: %target-swift-frontend -emit-ir %s -I %t -swift-version 4 -enable-objc-interop -D EXPORTED_AND_IMPLEMENTATION_ONLY | %FileCheck -check-prefix CHECK -check-prefix CHECK-NORMAL -check-prefix CHECK-NORMAL-%target-os -implicit-check-not BAD %s
// RUN: %target-swift-frontend -emit-ir %s -I %t -swift-version 4 -enable-objc-interop -D IMPLEMENTATION_ONLY_AND_EXPORTED | %FileCheck -check-prefix CHECK -check-prefix CHECK-NORMAL -check-prefix CHECK-NORMAL-%target-os -implicit-check-not BAD %s

// Linux uses a different autolinking mechanism, based on
// swift-autolink-extract. This file tests the Darwin mechanism.
/// UNSUPPORTED: autolink-extract

#if NORMAL_IMPORT
import autolinking_module_inferred

#elseif IMPLEMENTATION_ONLY_IMPORT
@_implementationOnly import autolinking_module_inferred

#elseif NORMAL_AND_IMPLEMENTATION_ONLY
import autolinking_module_inferred
@_implementationOnly import autolinking_module_inferred

#elseif IMPLEMENTATION_ONLY_AND_NORMAL
@_implementationOnly import autolinking_module_inferred
import autolinking_module_inferred

#elseif EXPORTED_AND_IMPLEMENTATION_ONLY
@_exported import autolinking_module_inferred
@_implementationOnly import autolinking_module_inferred

#elseif IMPLEMENTATION_ONLY_AND_EXPORTED
@_implementationOnly import autolinking_module_inferred
@_exported import autolinking_module_inferred

#else
#error("must pick an import mode to test")
#endif

bfunc()

// CHECK: !llvm.linker.options = !{

// CHECK-NORMAL-SAME: [[MODULE:![0-9]+]],
// CHECK-NORMAL-SAME: [[PUBLIC:![0-9]+]],
// CHECK-NORMAL-SAME: [[SWIFTONONESUPPORT:![0-9]+]],
// CHECK-NORMAL-SAME: [[SWIFTCORE:![0-9]+]],
// CHECK-NORMAL-windows-msvc-SAME: [[STDIO:![0-9]+]],

// This is the same set as the above, just in a different order due to a
// different traversal of the transitive import graph.
// CHECK-IMPL_ONLY-SAME: [[SWIFTONONESUPPORT:![0-9]+]],
// CHECK-IMPL_ONLY-SAME: [[SWIFTCORE:![0-9]+]],
// CHECK-IMPL_ONLY-windows-msvc-SAME: [[STDIO:![0-9]+]],
// CHECK-IMPL_ONLY-SAME: [[MODULE:![0-9]+]],
// CHECK-IMPL_ONLY-SAME: [[PUBLIC:![0-9]+]],

// CHECK-SAME: [[PRIVATE:![0-9]+]],
// CHECK-SAME: [[OTHER:![0-9]+]],
// CHECK-SAME: [[INDIRECT:![0-9]+]],
// CHECK-SAME: [[OTHER2:![0-9]+]],
// CHECK-SAME: [[OBJC:![0-9]+]]

// CHECK-SAME: }

// CHECK-DAG: [[MODULE]] = !{!{{"-lautolinking_module_inferred"|"/DEFAULTLIB:autolinking_module_inferred.lib"}}}
// CHECK-DAG: [[PUBLIC]] = !{!{{"-lautolinking_public"|"/DEFAULTLIB:autolinking_public.lib"}}}
// CHECK-DAG: [[SWIFTONONESUPPORT]] = !{!{{"-lswiftSwiftOnoneSupport"|"/DEFAULTLIB:swiftSwiftOnoneSupport.lib"}}}
// CHECK-DAG: [[SWIFTCORE]] = !{!{{"-lswiftCore"|"/DEFAULTLIB:swiftCore.lib"}}}
// CHECK-windows-msvc-DAG: [[STDIO]] = !{!"/DEFAULTLIB:legacy_stdio_definitions.lib"}
// CHECK-DAG: [[OTHER]] = !{!{{"-lautolinking_other"|"/DEFAULTLIB:autolinking_other.lib"}}}
// CHECK-DAG: [[OTHER2]] = !{!{{"-lautolinking_other2"|"/DEFAULTLIB:autolinking_other2.lib"}}}
// CHECK-DAG: [[OBJC]] = !{!{{"-lobjc"|"/DEFAULTLIB:objc.lib"}}}

// We don't actually care about these two. As long as we autolink the libraries
// that get used, we're okay.
// CHECK-DAG: [[PRIVATE]] = !{!{{"-lautolinking_private"|"/DEFAULTLIB:autolinking_private.lib"}}}
// CHECK-DAG: [[INDIRECT]] = !{!{{"-lautolinking_indirect"|"/DEFAULTLIB:autolinking_indirect.lib"}}}
