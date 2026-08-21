// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule -emit-module-interface-path %t/Test.swiftinterface -module-name Test  %s
// RUN: %target-swift-typecheck-module-from-interface(%t/Test.swiftinterface) -module-name Test

// Break the interface. The diagnostics expected by the '-verify' runs below are
// annotated in the interface itself.
// RUN: echo "public func bad() -> DoesNotExist // expected-error {{cannot find type 'DoesNotExist' in scope}} expected-error@1 {{failed to verify module interface of 'Test'}}" >> %t/Test.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/Test.swiftinterface) -module-name Test -verify -show-diagnostics-after-fatal

// RUN: echo "---" > %t/blocklist.yml
// RUN: echo "DowngradeInterfaceVerificationFailure:" >> %t/blocklist.yml
// RUN: echo "  ModuleName:" >> %t/blocklist.yml
// RUN: echo "    - FooBar" >> %t/blocklist.yml

// RUN: %target-swift-typecheck-module-from-interface(%t/Test.swiftinterface) -module-name Test -blocklist-file %t/blocklist.yml -verify -show-diagnostics-after-fatal

// RUN: echo "    - Test" >> %t/blocklist.yml

// RUN: %target-swift-typecheck-module-from-interface(%t/Test.swiftinterface) -module-name Test -downgrade-typecheck-interface-error
// RUN: %target-swift-typecheck-module-from-interface(%t/Test.swiftinterface) -module-name Test -blocklist-file %t/blocklist.yml

// RUN: %target-swift-typecheck-module-from-interface(%t/Test.swiftinterface) -module-name Test -blocklist-file %t/blocklist.yml &> %t/notes.txt
// RUN: %FileCheck -check-prefix CHECK-NOTES --input-file %t/notes.txt %s
// CHECK-NOTES: note: textual interface for Test is blocklisted as broken; interface verification errors are downgraded to warnings

// '-no-downgrade-typecheck-interface-error' disregards the blocklist.
// RUN: %target-swift-typecheck-module-from-interface(%t/Test.swiftinterface) -module-name Test -blocklist-file %t/blocklist.yml -no-downgrade-typecheck-interface-error -verify -show-diagnostics-after-fatal

// The last of the downgrade flags wins.
// RUN: %target-swift-typecheck-module-from-interface(%t/Test.swiftinterface) -module-name Test -downgrade-typecheck-interface-error -no-downgrade-typecheck-interface-error -verify -show-diagnostics-after-fatal
// RUN: %target-swift-typecheck-module-from-interface(%t/Test.swiftinterface) -module-name Test -no-downgrade-typecheck-interface-error -downgrade-typecheck-interface-error

public func foo() {}
