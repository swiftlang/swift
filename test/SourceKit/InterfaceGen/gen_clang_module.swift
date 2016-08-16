import Foo

var x: FooClassBase

// REQUIRES: objc_interop

// RUN: rm -rf %t.overlays
// RUN: mkdir %t.overlays
//
// RUN: %swift -emit-module -o %t.overlays -F %S/../Inputs/libIDE-mock-sdk %S/Inputs/Foo.swift
//
// RUN: %sourcekitd-test -req=interface-gen -module Foo -- -I %t.overlays -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt %clang-importer-sdk > %t.response
// RUN: diff -u %s.response %t.response

// RUN: %sourcekitd-test -req=interface-gen -module Foo.FooSub -- -I %t.overlays -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt %clang-importer-sdk > %t.sub.response
// RUN: diff -u %s.sub.response %t.sub.response

// RUN: %sourcekitd-test -req=interface-gen -module FooHelper -- -I %t.overlays -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt %clang-importer-sdk > %t.helper.response
// RUN: diff -u %s.helper.response %t.helper.response

// RUN: %sourcekitd-test -req=interface-gen -module FooHelper.FooHelperExplicit -- -I %t.overlays \
// RUN:         -F %S/../Inputs/libIDE-mock-sdk  %mcp_opt %clang-importer-sdk > %t.helper.explicit.response
// RUN: diff -u %s.helper.explicit.response %t.helper.explicit.response

// RUN: %sourcekitd-test -req=interface-gen-open -module Foo -- -I %t.overlays -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt %clang-importer-sdk \
// RUN:      == -req=cursor -pos=203:67 | %FileCheck -check-prefix=CHECK1 %s
// The cursor points to 'FooClassBase' inside the list of base classes, see 'gen_clang_module.swift.response'

// RUN: %sourcekitd-test -req=interface-gen-open -module Foo -- -I %t.overlays -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt %clang-importer-sdk \
// RUN:   == -req=cursor -pos=3:11 %s -- %s -I %t.overlays -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt %clang-importer-sdk | %FileCheck -check-prefix=CHECK1 %s

// CHECK1: source.lang.swift.ref.class ({{.*}}Foo.framework/Headers/Foo.h:146:12-146:24)
// CHECK1: FooClassBase
// CHECK1: c:objc(cs)FooClassBase
// CHECK1: Foo{{$}}
// CHECK1-NEXT: /<interface-gen>

// RUN: %sourcekitd-test -req=interface-gen-open -module Foo -- -I %t.overlays -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt %clang-importer-sdk \
// RUN:      == -req=cursor -pos=230:20 | %FileCheck -check-prefix=CHECK2 %s
// The cursor points inside the interface, see 'gen_clang_module.swift.response'

// CHECK2: source.lang.swift.decl.function.method.instance ({{.*}}Foo.framework/Headers/Foo.h:169:10-169:27)
// CHECK2: fooInstanceFunc0
// CHECK2: c:objc(cs)FooClassDerived(im)fooInstanceFunc0
// CHECK2: Foo{{$}}
// CHECK2-NEXT: /<interface-gen>

// RUN: %sourcekitd-test -req=interface-gen-open -module Foo -- -I %t.overlays -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt %clang-importer-sdk \
// RUN:      == -req=find-usr -usr "c:objc(cs)FooClassDerived(im)fooInstanceFunc0" | %FileCheck -check-prefix=CHECK-USR %s
// The returned line:col points inside the interface, see 'gen_clang_module.swift.response'

// CHECK-USR: (230:15-230:33)

// RUN: %sourcekitd-test -req=interface-gen-open -module Foo -- -I %t.overlays -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt %clang-importer-sdk \
// RUN:   == -req=find-interface -module Foo -- %s -I %t.overlays -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt %clang-importer-sdk | %FileCheck -check-prefix=CHECK-IFACE %s

// CHECK-IFACE: DOC: (/<interface-gen>)
// CHECK-IFACE: ARGS: [-target x86_64-{{.*}} -sdk {{.*}} -F {{.*}}/libIDE-mock-sdk -I {{.*}}.overlays {{.*}} -module-cache-path {{.*}} ]

// RUN: %sourcekitd-test -req=interface-gen-open -module Foo -- -I %t.overlays -F %S/../Inputs/libIDE-mock-sdk \
// RUN:         %mcp_opt %clang-importer-sdk \
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
