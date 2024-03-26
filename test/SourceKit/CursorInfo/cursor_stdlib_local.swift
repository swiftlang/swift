// This test ensures that we don't consider the stdlib as a system library if
// it's a module that we're building outside of the resource path.

//--- Swift.swift
public enum E {
  case a, b, c
}

//--- Client.swift
func test(_ x: E) {
  if case .a = x {}
}

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// RUN: %empty-directory(%t/modules)
// RUN: %empty-directory(%t/modulecache)

// RUN: %target-swift-frontend -emit-module -module-name Swift -parse-stdlib -swift-version 5 -enable-library-evolution -emit-module-interface-path %t/modules/Swift.swiftinterface -emit-module-source-info-path %t/modules/Swift.swiftsourceinfo %t/Swift.swift -o /dev/null

// RUN: %sourcekitd-test -req=cursor -pos=10:16 %t/Client.swift -- -I %t/modules -module-cache-path %t/modulecache -target %target-triple %t/Client.swift | %FileCheck %s -check-prefix=CHECK1
// CHECK1:      source.lang.swift.ref.enum ({{.*}}Swift.swift:5:13-5:14)
// CHECK1-NEXT: E
// CHECK1-NEXT: s:s1EO
// CHECK1-NOT:  SYSTEM

// RUN: %sourcekitd-test -req=cursor -pos=11:12 %t/Client.swift -- -I %t/modules -module-cache-path %t/modulecache -target %target-triple %t/Client.swift | %FileCheck %s -check-prefix=CHECK2
// CHECK2:      source.lang.swift.ref.enumelement ({{.*}}Swift.swift:6:8-6:9)
// CHECK2-NEXT: a
// CHECK2-NEXT: s:s1EO1ayA2BmF
// CHECK2-NOT:  SYSTEM

// Now try again with a swiftmodule.

// RUN: %empty-directory(%t/modules)
// RUN: %empty-directory(%t/modulecache)

// RUN: %target-swift-frontend -emit-module -module-name Swift -parse-stdlib -swift-version 5 -enable-library-evolution %t/Swift.swift -emit-module-path %t/modules/Swift.swiftmodule -emit-module-source-info-path %t/modules/Swift.swiftsourceinfo

// RUN: %sourcekitd-test -req=cursor -pos=10:16 %t/Client.swift -- -I %t/modules -target %target-triple %t/Client.swift | %FileCheck %s -check-prefix=CHECK1
// RUN: %sourcekitd-test -req=cursor -pos=11:12 %t/Client.swift -- -I %t/modules -target %target-triple %t/Client.swift | %FileCheck %s -check-prefix=CHECK2
