// RUN: %empty-directory(%t)

// Test that -Werror OptionObsoletedByModuleSelectors turns the obsoleted option
// warning into an error.

// RUN: %empty-directory(%t/PreserveTypesAsWritten)
// RUN: not %target-swift-emit-module-interface(%t/PreserveTypesAsWritten/TestCase.swiftinterface) %s -module-name TestCase -target %target-stable-abi-triple -module-interface-preserve-types-as-written -enable-module-selectors-in-module-interface -Werror OptionObsoletedByModuleSelectors 2>%t/PreserveTypesAsWritten/stderr.txt
// RUN: %FileCheck --input-file %t/PreserveTypesAsWritten/stderr.txt %s --check-prefix CHECK-PRESERVE

// Test that -Werror OptionObsoletedByModuleSelectors turns the warning for
// -alias-module-names-in-module-interface into an error.

// RUN: %empty-directory(%t/AliasModuleNames)
// RUN: not %target-swift-emit-module-interface(%t/AliasModuleNames/TestCase.swiftinterface) %s -module-name TestCase -target %target-stable-abi-triple -alias-module-names-in-module-interface -enable-module-selectors-in-module-interface -Werror OptionObsoletedByModuleSelectors 2>%t/AliasModuleNames/stderr.txt
// RUN: %FileCheck --input-file %t/AliasModuleNames/stderr.txt %s --check-prefix CHECK-ALIAS

// Test that -Werror OptionObsoletedByModuleSelectors does not cause failure
// when no obsoleted option is passed.

// RUN: %empty-directory(%t/Clean)
// RUN: %target-swift-emit-module-interface(%t/Clean/TestCase.swiftinterface) %s -module-name TestCase -target %target-stable-abi-triple -Werror OptionObsoletedByModuleSelectors 2>%t/Clean/stderr.txt
// RUN: %FileCheck --input-file %t/Clean/stderr.txt %s --allow-empty --check-prefix CHECK-CLEAN

// CHECK-PRESERVE: error: ignoring '-module-interface-preserve-types-as-written'; this option has been obsoleted by module selectors (add '-disable-module-selectors-in-module-interface' to restore original behavior)

// CHECK-ALIAS: error: ignoring '-alias-module-names-in-module-interface'; this option has been obsoleted by module selectors (add '-disable-module-selectors-in-module-interface' to restore original behavior)

// CHECK-CLEAN-NOT: error:
// CHECK-CLEAN-NOT: warning:

public struct Foo {}
