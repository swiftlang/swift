// RUN: %empty-directory(%t/mcp)

// Build modules imported by this file.
// RUN: %empty-directory(%t/lib)
// RUN: %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/mcp -emit-module-interface-path %t/lib/ShadowyHorror.swiftinterface %S/Inputs/ShadowyHorror.swift -enable-library-evolution -module-name ShadowyHorror -swift-version 5
// RUN: %target-swift-frontend -typecheck -parse-stdlib -emit-module-interface-path %t/lib/TestModule.swiftinterface %S/Inputs/TestModule.swift -enable-library-evolution -module-name TestModule -swift-version 5

// Build this file as a module and check that it emits the expected warnings.
// RUN: %empty-directory(%t/subtest-1)
// RUN: %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/mcp -emit-module-interface-path %t/subtest-1/module_shadowing.swiftinterface %s -enable-library-evolution -module-name module_shadowing -I %t/lib -swift-version 5 2>&1 | %FileCheck --check-prefix OTHER --implicit-check-not TestModule %s

// Make sure that preserve-types-as-written disables this warning.
// RUN: %empty-directory(%t/subtest-2)
// RUN: %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/mcp -emit-module-interface-path %t/subtest-2/module_shadowing.swiftinterface %s -enable-library-evolution -module-name module_shadowing -I %t/lib -module-interface-preserve-types-as-written -swift-version 5 -verify

// Build this module in a different configuration where it will shadow itself.
// RUN: %empty-directory(%t/subtest-3)
// RUN: %target-swift-frontend -typecheck -parse-stdlib -module-cache-path %t/mcp -emit-module-interface-path %t/subtest-3/ShadowyHorror.swiftinterface %s -enable-library-evolution -module-name ShadowyHorror -DSELF_SHADOW -swift-version 5 2>&1 | %FileCheck --check-prefix SELF --implicit-check-not TestModule %s

// Verify that the module-shadowing bugs we're trying to address haven't been fixed. (SR-898)
// RUN: not %target-swift-frontend -typecheck-module-from-interface %t/subtest-1/module_shadowing.swiftinterface -module-cache-path %t/mcp -I %t/lib -module-name module_shadowing 2>&1 | %FileCheck --check-prefix VERIFICATION %s
// RUN: %target-swift-frontend -typecheck-module-from-interface %t/subtest-2/module_shadowing.swiftinterface -module-cache-path %t/mcp -I %t/lib -module-name module_shadowing
// RUN: not %target-swift-frontend -typecheck-module-from-interface %t/subtest-3/ShadowyHorror.swiftinterface -module-cache-path %t/mcp -I %t/lib -module-name ShadowyHorror 2>&1 | %FileCheck --check-prefix VERIFICATION %s

#if !SELF_SHADOW
import ShadowyHorror
// OTHER-DAG: ShadowyHorror.module_shadowing:{{[0-9]+:[0-9]+}}: warning: public class 'ShadowyHorror.module_shadowing' shadows module 'module_shadowing', which is permitted by Swift but may stop other compiler versions from importing module 'module_shadowing' or its clients; please work around this language bug (SR-898) by renaming either the class 'ShadowyHorror.module_shadowing' or the module 'module_shadowing', or by passing the '-Xfrontend -module-interface-preserve-types-as-written' flags to the Swift compiler{{$}}

@_implementationOnly import TestModule
#endif

public struct ShadowyHorror {
  // OTHER-DAG: module_shadowing.swift:[[@LINE-1]]:15: warning: public struct 'module_shadowing.ShadowyHorror' shadows module 'ShadowyHorror', which is permitted by Swift but may stop other compiler versions from importing module 'module_shadowing' or its clients; please work around this language bug (SR-898) by renaming either the struct 'module_shadowing.ShadowyHorror' or the module 'ShadowyHorror', or by passing the '-Xfrontend -module-interface-preserve-types-as-written' flags to the Swift compiler{{$}}
  // SELF: module_shadowing.swift:[[@LINE-2]]:15: warning: public struct 'ShadowyHorror.ShadowyHorror' shadows module 'ShadowyHorror', which is permitted by Swift but may stop other compiler versions from importing module 'ShadowyHorror' or its clients; please work around this language bug (SR-898) by renaming either the struct 'ShadowyHorror.ShadowyHorror' or the module 'ShadowyHorror', or by passing the '-Xfrontend -module-interface-preserve-types-as-written' flags to the Swift compiler{{$}}

  init() {}
  public var property: ShadowyHorror { ShadowyHorror() }
  // VERIFICATION: error: 'ShadowyHorror' is not a member type of {{class|struct}} 'ShadowyHorror.{{ShadowyHorror|module_shadowing}}'
  // VERIFICATION: error: failed to verify module interface of '{{ShadowyHorror|module_shadowing}}' due to the errors above;
}

public struct TestModule {}
