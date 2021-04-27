// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t.module-cache)
//
// RUN: %target-swift-frontend -typecheck -disable-implicit-concurrency-module-import -emit-module-interface-path %t/label.swiftinterface -enable-library-evolution -module-cache-path %t.module-cache %s
// RUN: %FileCheck --check-prefix=SWIFT_INTERFACE %s < %t/label.swiftinterface
// RUN: %sourcekitd-test -req=doc-info -module label -- -Xfrontend -disable-implicit-concurrency-module-import -I %t -target %target-triple -module-cache-path %t.module-cache > %t.response
// RUN: %diff -u %s.response %t.response
//
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t.module-cache)
// RUN: %target-swift-frontend -emit-module %s -disable-implicit-concurrency-module-import -module-name label -emit-module-path %t/label.swiftmodule -module-cache-path %t.module-cache
// RUN: %sourcekitd-test -req=doc-info -module label -- -Xfrontend -disable-implicit-concurrency-module-import -I %t -target %target-triple -module-cache-path %t.module-cache> %t.response
// RUN: %diff -u %s.response %t.response

public func foo(_ callback: (_ myInternalParam: Int) -> Void) {}

// SWIFT_INTERFACE: import Swift
// SWIFT_INTERFACE: public func foo(_ callback: (_ myInternalParam: Swift.Int) -> Swift.Void)
