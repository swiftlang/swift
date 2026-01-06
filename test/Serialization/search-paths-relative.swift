// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/secret)
// RUN: %target-swift-frontend -emit-module -o %t/secret %S/Inputs/struct_with_operators.swift
// RUN: %empty-directory(%t/Frameworks/has_alias.framework/Modules/has_alias.swiftmodule)
// RUN: %target-swift-frontend -emit-module -o %t/Frameworks/has_alias.framework/Modules/has_alias.swiftmodule/%target-swiftmodule-name %S/Inputs/alias.swift -module-name has_alias

// Output paths differ in the new driver, so force SWIFT_USE_OLD_DRIVER for now.
// RUN: cd %t/secret && env SWIFT_USE_OLD_DRIVER=1 %target-swiftc_driver -emit-module -o %t/has_xref.swiftmodule -I . -F ../Frameworks -parse-as-library %S/Inputs/has_xref.swift %S/../Inputs/empty.swift -Xfrontend -serialize-debugging-options -Xcc -ivfsoverlay -Xcc %S/../Inputs/unextended-module-overlay.yaml -Xcc -DDUMMY
// RUN: cd %t/secret && env SWIFT_USE_OLD_DRIVER=1 %target-swiftc_driver -emit-module -o %t/explicit.swiftmodule -parse-stdlib -parse-as-library %S/../Inputs/empty.swift -Xfrontend -disable-implicit-swift-modules -Xfrontend -serialize-debugging-options -Xcc -ivfsoverlay -Xcc %S/../Inputs/unextended-module-overlay.yaml -Xcc -DDUMMY
// RUN: %target-swift-frontend %s -typecheck -I %t

// Ensure that in Swift 6 mode we do not read out search paths, thus are no longer able to
// locate transitive dependencies with them
// RUN: not %target-swift-frontend %s -typecheck -I %t -swift-version 6 &> %t/swift_6_output.txt
// RUN: %FileCheck -check-prefix=SWIFT6 %s < %t/swift_6_output.txt

// Check the actual serialized search paths.
// RUN: llvm-bcanalyzer -dump %t/has_xref.swiftmodule > %t/has_xref.swiftmodule.txt
// RUN: llvm-bcanalyzer -dump %t/explicit.swiftmodule > %t/explicit.swiftmodule.txt
// RUN: %FileCheck %s < %t/has_xref.swiftmodule.txt
// RUN: %FileCheck -check-prefix=NEGATIVE %s < %t/has_xref.swiftmodule.txt
// RUN: %FileCheck -check-prefix=EXPLICIT %s < %t/explicit.swiftmodule.txt

import has_xref

numeric(42)

// CHECK-LABEL: <OPTIONS_BLOCK
// CHECK: <XCC abbrevid={{[0-9]+}}/> blob data = '-working-directory'
// CHECK: <XCC abbrevid={{[0-9]+}}/> blob data = '{{.+}}{{/|\\}}secret'
// CHECK: <XCC abbrevid={{[0-9]+}}/> blob data = '-DDUMMY'
// CHECK: </OPTIONS_BLOCK>

// CHECK-LABEL: <INPUT_BLOCK
// CHECK: <SEARCH_PATH abbrevid={{[0-9]+}} op0=1 op1=0/> blob data = '{{.+}}{{/|\\}}secret{{/|\\}}../Frameworks'
// CHECK: <SEARCH_PATH abbrevid={{[0-9]+}} op0=0 op1=0/> blob data = '{{.+}}{{/|\\}}secret{{/|\\}}.'
// CHECK: </INPUT_BLOCK>

// NEGATIVE-NOT: '.'
// NEGATIVE-NOT: '../Frameworks'
// This should be filtered out.
// NEGATIVE-NOT: -ivfsoverlay
// NEGATIVE-NOT: unextended-module-overlay.yaml
// EXPLICIT: -ivfsoverlay
// EXPLICIT: unextended-module-overlay.yaml
// EXPLICIT: -DDUMMY

// SWIFT6: error: missing required modules: 'has_alias', 'struct_with_operators'
