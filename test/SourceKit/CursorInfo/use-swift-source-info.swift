import Foo
func bar() {
  foo()
}

// FIXME: Rmove REQUIRES rdar://problem/60096971
// REQUIRES: rdar60096971

// RUN: %empty-directory(%t)
// RUN: echo "/// Some doc" >> %t/Foo.swift
// RUN: echo "public func foo() { }" >> %t/Foo.swift
// RUN: %target-swift-frontend -enable-batch-mode -emit-module -emit-module-doc -emit-module-path %t/Foo.swiftmodule %t/Foo.swift -module-name Foo -emit-module-source-info-path %t/Foo.swiftsourceinfo -emit-module-doc-path %t/Foo.swiftdoc
//
// Test setting optimize for ide to false
// RUN: %sourcekitd-test -req=global-config -req-opts=optimize_for_ide=0 == -req=cursor -pos=3:3 %s -- -I %t -target %target-triple %s | %FileCheck --check-prefixes=BOTH,WITH %s
//
// Test setting optimize for ide to true
// RUN: %sourcekitd-test -req=global-config -req-opts=optimize_for_ide=1 == -req=cursor -pos=3:3 %s -- -I %t -target %target-triple %s | %FileCheck --check-prefixes=BOTH,WITHOUT %s
//
// Test sourcekitd-test's default global configuration request (optimize for ide is true)
// RUN: %sourcekitd-test -req=cursor -pos=3:3 %s -- -I %t -target %target-triple %s | %FileCheck --check-prefixes=BOTH,WITHOUT %s
//
// Test without sending any global configuration request to check the sevice's default settings (optimize for ide is false)
// RUN: %sourcekitd-test -suppress-config-request -req=cursor -pos=3:3 %s -- -I %t -target %target-triple %s | %FileCheck --check-prefixes=BOTH,WITH %s

// WITH: source.lang.swift.ref.function.free ({{.*}}/Foo.swift:2:13-2:16)
// WITHOUT: source.lang.swift.ref.function.free ()
// BOTH: foo()
// BOTH-NEXT: s:3Foo3fooyyF
// BOTH-NEXT: () -> ()
// BOTH-NEXT: $syycD
// BOTH-NEXT: Foo
