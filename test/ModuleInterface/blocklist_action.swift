// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule -emit-module-interface-path %t/Test.swiftinterface -module-name Test  %s
// RUN: %target-swift-typecheck-module-from-interface(%t/Test.swiftinterface) -module-name Test

// RUN: echo "<<<<<>>>>>>>>" >> %t/Test.swiftinterface
// RUN: not %target-swift-typecheck-module-from-interface(%t/Test.swiftinterface) -module-name Test

// RUN: echo "---" > %t/blocklist.yml
// RUN: echo "DowngradeInterfaceVerificationFailure:" >> %t/blocklist.yml
// RUN: echo "  ModuleName:" >> %t/blocklist.yml
// RUN: echo "    - FooBar" >> %t/blocklist.yml

// RUN: not %target-swift-typecheck-module-from-interface(%t/Test.swiftinterface) -module-name Test -blocklist-file %t/blocklist.yml

// RUN: echo "    - Test" >> %t/blocklist.yml

// RUN: %target-swift-typecheck-module-from-interface(%t/Test.swiftinterface) -module-name Test -downgrade-typecheck-interface-error
// RUN: %target-swift-typecheck-module-from-interface(%t/Test.swiftinterface) -module-name Test -blocklist-file %t/blocklist.yml

public func foo() {}
