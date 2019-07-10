import Foo

var x: FooClassBase

// REQUIRES: objc_interop

// FIXME: the test output we're comparing to is specific to macOS.
// REQUIRES-ANY: OS=macosx

// RUN: %empty-directory(%t.overlays)
// RUN: %empty-directory(%t)
// RUN: %build-clang-importer-objc-overlays
//
// RUN: %target-swift-frontend -emit-module -o %t.overlays -F %S/../Inputs/libIDE-mock-sdk %S/Inputs/Foo.swift
//
// RUN: %sourcekitd-test -req=interface-gen -module Foo -- -I %t.overlays -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt -target %target-triple %clang-importer-sdk-nosource -I %t > %t.response
// RUN: diff -u %s.response %t.response

// RUN: %sourcekitd-test -req=interface-gen -module Foo.FooSub -- -I %t.overlays -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt -target %target-triple %clang-importer-sdk-nosource -I %t > %t.sub.response
// RUN: diff -u %s.sub.response %t.sub.response

// RUN: %sourcekitd-test -req=interface-gen -module FooHelper -- -I %t.overlays -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt -target %target-triple %clang-importer-sdk-nosource -I %t > %t.helper.response
// RUN: diff -u %s.helper.response %t.helper.response

// RUN: %sourcekitd-test -req=interface-gen -module FooHelper.FooHelperExplicit -- -I %t.overlays \
// RUN:         -F %S/../Inputs/libIDE-mock-sdk  %mcp_opt -target %target-triple %clang-importer-sdk-nosource -I %t > %t.helper.explicit.response
// RUN: diff -u %s.helper.explicit.response %t.helper.explicit.response

// RUN: %sourcekitd-test -req=interface-gen-open -module Foo -- -I %t.overlays -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt -target %target-triple %clang-importer-sdk-nosource -I %t \
// RUN:      == -req=cursor -pos=205:67 | %FileCheck -check-prefix=CHECK1 %s
// The cursor points to 'FooClassBase' inside the list of base classes, see 'gen_clang_module.swift.response'

// RUN: %sourcekitd-test -req=interface-gen-open -module Foo -- -I %t.overlays -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt -target %target-triple %clang-importer-sdk-nosource -I %t \
// RUN:   == -req=cursor -pos=3:11 %s -- %s -I %t.overlays -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt -target %target-triple %clang-importer-sdk-nosource -I %t | %FileCheck -check-prefix=CHECK1 %s

// CHECK1: source.lang.swift.ref.class ({{.*}}Foo.framework/Headers/Foo.h:147:12-147:24)
// CHECK1: FooClassBase
// CHECK1: c:objc(cs)FooClassBase
// CHECK1: Foo{{$}}
// CHECK1-NEXT: /<interface-gen>

// RUN: %sourcekitd-test -req=interface-gen-open -module Foo -- -I %t.overlays -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt -target %target-triple %clang-importer-sdk-nosource -I %t \
// RUN:      == -req=cursor -pos=232:20 | %FileCheck -check-prefix=CHECK2 %s
// The cursor points inside the interface, see 'gen_clang_module.swift.response'

// CHECK2: source.lang.swift.decl.function.method.instance ({{.*}}Foo.framework/Headers/Foo.h:170:10-170:27)
// CHECK2: fooInstanceFunc0
// CHECK2: c:objc(cs)FooClassDerived(im)fooInstanceFunc0
// CHECK2: Foo{{$}}
// CHECK2-NEXT: /<interface-gen>

// RUN: %sourcekitd-test -req=interface-gen-open -module Foo -- -I %t.overlays -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt -target %target-triple %clang-importer-sdk-nosource -I %t \
// RUN:      == -req=find-usr -usr "c:objc(cs)FooClassDerived(im)fooInstanceFunc0" | %FileCheck -check-prefix=CHECK-USR %s
// The returned line:col points inside the interface, see 'gen_clang_module.swift.response'

// CHECK-USR: (232:15-232:33)

// RUN: %sourcekitd-test -req=interface-gen-open -module Foo -- -I %t.overlays -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt -target %target-triple %clang-importer-sdk-nosource -I %t \
// RUN:   == -req=find-interface -module Foo -- %s -I %t.overlays -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt -target %target-triple %clang-importer-sdk-nosource -I %t | %FileCheck -check-prefix=CHECK-IFACE %s

// CHECK-IFACE: DOC: (/<interface-gen>)
// CHECK-IFACE: ARGS: [-target x86_64-{{.*}} -sdk {{.*}} -F {{.*}}/libIDE-mock-sdk -I {{.*}}.overlays {{.*}} -module-cache-path {{.*}} ]

// RUN: %sourcekitd-test -req=interface-gen-open -module Foo -- -I %t.overlays -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt -target %target-triple %clang-importer-sdk-nosource -I %t \
// RUN:      == -req=cursor -pos=1:8 == -req=cursor -pos=1:12 \
// RUN:      == -req=cursor -pos=2:10 \
// RUN:      == -req=cursor -pos=3:10 | %FileCheck -check-prefix=CHECK-IMPORT %s
// The cursors point to module names inside the imports, see 'gen_clang_module.swift.response'

// CHECK-IMPORT: 	  source.lang.swift.ref.module ()
// CHECK-IMPORT-NEXT: Foo{{$}}
// CHECK-IMPORT-NEXT: Foo{{$}}
// CHECK-IMPORT: 	  source.lang.swift.ref.module ()
// CHECK-IMPORT-NEXT: FooSub{{$}}
// CHECK-IMPORT-NEXT: Foo.FooSub{{$}}
// CHECK-IMPORT: 	  source.lang.swift.ref.module ()
// CHECK-IMPORT-NEXT: Foo{{$}}
// CHECK-IMPORT-NEXT: Foo{{$}}
// CHECK-IMPORT: 	  source.lang.swift.ref.module ()
// CHECK-IMPORT-NEXT: FooHelper{{$}}
// CHECK-IMPORT-NEXT: FooHelper{{$}}

// RUN: %sourcekitd-test -req=interface-gen -module APINotesTests -- -swift-version 4 -F %S/Inputs/mock-sdk \
// RUN:         %mcp_opt -target %target-triple %clang-importer-sdk-nosource > %t.apinotes_swift3.response
// RUN: diff -u %s.apinotes_swift3.response %t.apinotes_swift3.response
// RUN: %sourcekitd-test -req=interface-gen -module APINotesTests -- -swift-version 5 -F %S/Inputs/mock-sdk \
// RUN:         %mcp_opt -target %target-triple %clang-importer-sdk-nosource > %t.apinotes_swift4.response
// RUN: diff -u %s.apinotes_swift4.response %t.apinotes_swift4.response
