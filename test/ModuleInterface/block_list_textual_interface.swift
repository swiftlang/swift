// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t.sdk)
// RUN: %empty-directory(%t/modulecache)
// RUN: %target-swift-frontend -emit-module -o %t.sdk/Foo.swiftmodule -emit-module-interface-path %t.sdk/Foo.swiftinterface -module-name Foo  %s -D FOO -enable-library-evolution
// RUN: %target-swift-frontend -emit-module -o %t.sdk/Bar.swiftmodule -emit-module-interface-path %t.sdk/Bar.swiftinterface -module-name Bar  %s -D BAR -enable-library-evolution -I %t.sdk

#if FOO

public func fooFunc() {}

#elseif BAR

import Foo
public func barFunc() { fooFunc() }

#elseif BAZ

import Bar
public func bazFunc() { barFunc() }

#endif


// RUN: echo "---" > %t/blocklist.yml
// RUN: echo "DowngradeInterfaceVerificationFailure:" >> %t/blocklist.yml
// RUN: echo "  ModuleName:" >> %t/blocklist.yml
// RUN: echo "    - TestBlocklistedModule # For tests" >> %t/blocklist.yml
// RUN: echo "ShouldUseTextualModule:" >> %t/blocklist.yml
// RUN: echo "  ModuleName:" >> %t/blocklist.yml
// RUN: echo "    - Foo # for tests" >> %t/blocklist.yml

// RUN: %target-swift-frontend -typecheck %s -D BAZ -I %t.sdk -blocklist-file %t/blocklist.yml -module-cache-path %t/modulecache -Rmodule-interface-rebuild &> %t/notes.txt
// RUN: %FileCheck -check-prefix CHECK-NOTES --input-file %t/notes.txt %s
// CHECK-NOTES: Foo.swiftmodule' was ignored because the module name is blocklisted
