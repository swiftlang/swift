// RUN: %empty-directory(%t/mcp)

// Build modules imported by this file.
// RUN: %empty-directory(%t/lib)
// RUN: %target-swift-emit-module-interface(%t/lib/ShadowyHorror.swiftinterface) %S/Inputs/ShadowyHorror.swift -parse-stdlib -module-cache-path %t/mcp -module-name ShadowyHorror
// RUN: %target-swift-typecheck-module-from-interface(%t/lib/ShadowyHorror.swiftinterface) -module-name ShadowyHorror
// RUN: %target-swift-emit-module-interface(%t/lib/TestModule.swiftinterface) %S/Inputs/TestModule.swift -parse-stdlib -module-name TestModule
// RUN: %target-swift-typecheck-module-from-interface(%t/lib/TestModule.swiftinterface) -module-name TestModule

// Build this file as a module and check that it emits the expected warnings.
// RUN: %empty-directory(%t/subtest-1)
// RUN: %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/mcp -emit-module-interface-path %t/subtest-1/module_shadowing.swiftinterface %s -enable-library-evolution -module-name module_shadowing -I %t/lib -swift-version 5 2>&1 | %FileCheck --check-prefix OTHER --implicit-check-not TestModule %s

// Make sure that preserve-types-as-written disables this warning.
// RUN: %empty-directory(%t/subtest-2)
// RUN: %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/mcp -emit-module-interface-path %t/subtest-2/module_shadowing.swiftinterface %s -enable-library-evolution -module-name module_shadowing -I %t/lib -module-interface-preserve-types-as-written -swift-version 5 -verify

// alias-module-names-in-module-interface also disables this warning.
// RUN: %empty-directory(%t/subtest-2)
// RUN: %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/mcp -emit-module-interface-path %t/subtest-2/module_shadowing.swiftinterface %s -enable-library-evolution -module-name module_shadowing -I %t/lib -alias-module-names-in-module-interface -swift-version 5 -verify

// Build this module in a different configuration where it will shadow itself.
// RUN: %empty-directory(%t/subtest-3)
// RUN: %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/mcp -emit-module-interface-path %t/subtest-3/ShadowyHorror.swiftinterface %s -enable-library-evolution -module-name ShadowyHorror -DSELF_SHADOW -swift-version 5 2>&1 | %FileCheck --check-prefix SELF --implicit-check-not TestModule %s

// FIXME: Verify that the module-shadowing bugs we're trying to address haven't
// been fixed (https://github.com/apple/swift/issues/43510).
// RUN: not %target-swift-frontend -typecheck-module-from-interface %t/subtest-1/module_shadowing.swiftinterface -module-cache-path %t/mcp -I %t/lib -module-name module_shadowing 2>&1 | %FileCheck --check-prefix VERIFICATION %s
// RUN: %target-swift-frontend -typecheck-module-from-interface %t/subtest-2/module_shadowing.swiftinterface -module-cache-path %t/mcp -I %t/lib -module-name module_shadowing
// RUN: not %target-swift-frontend -typecheck-module-from-interface %t/subtest-3/ShadowyHorror.swiftinterface -module-cache-path %t/mcp -I %t/lib -module-name ShadowyHorror 2>&1 | %FileCheck --check-prefix VERIFICATION %s

#if !SELF_SHADOW
import ShadowyHorror
// OTHER-DAG: ShadowyHorror.module_shadowing:{{[0-9]+:[0-9]+}}: warning: public class 'ShadowyHorror.module_shadowing' shadows module 'module_shadowing', which may cause failures when importing 'module_shadowing' or its clients in some configurations; please rename either the class 'ShadowyHorror.module_shadowing' or the module 'module_shadowing', or see https://github.com/apple/swift/issues/56573 for workarounds{{$}}

internal import TestModule
#endif

public struct ShadowyHorror {
  // OTHER-DAG: module_shadowing.swift:[[@LINE-1]]:15: warning: public struct 'module_shadowing.ShadowyHorror' shadows module 'ShadowyHorror', which may cause failures when importing 'module_shadowing' or its clients in some configurations; please rename either the struct 'module_shadowing.ShadowyHorror' or the module 'ShadowyHorror', or see https://github.com/apple/swift/issues/56573 for workarounds{{$}}
  // SELF: module_shadowing.swift:[[@LINE-2]]:15: warning: public struct 'ShadowyHorror.ShadowyHorror' shadows module 'ShadowyHorror', which may cause failures when importing 'ShadowyHorror' or its clients in some configurations; please rename either the struct 'ShadowyHorror.ShadowyHorror' or the module 'ShadowyHorror', or see https://github.com/apple/swift/issues/56573 for workarounds{{$}}

  init() {}
  public var property: ShadowyHorror { ShadowyHorror() }
  // VERIFICATION: error: 'ShadowyHorror' is not a member type of {{class|struct}} 'ShadowyHorror.{{ShadowyHorror|module_shadowing}}'
  // VERIFICATION: error: failed to verify module interface of '{{ShadowyHorror|module_shadowing}}' due to the errors above;
}

public struct TestModule {}
