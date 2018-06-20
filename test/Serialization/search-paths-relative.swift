// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/secret)
// RUN: %target-swift-frontend -emit-module -o %t/secret %S/Inputs/struct_with_operators.swift
// RUN: %empty-directory(%t/Frameworks/has_alias.framework/Modules/has_alias.swiftmodule)
// RUN: %target-swift-frontend -emit-module -o %t/Frameworks/has_alias.framework/Modules/has_alias.swiftmodule/%target-swiftmodule-name %S/Inputs/alias.swift -module-name has_alias

// RUN: cd %t/secret && %target-swiftc_driver -emit-module \
// RUN:   -o %t/has_xref.swiftmodule -I . -F ../Frameworks \
// RUN:   -parse-as-library %S/Inputs/has_xref.swift %S/../Inputs/empty.swift \
// RUN:   -Xfrontend -serialize-debugging-options \
// RUN:   -Xcc -ivfsoverlay \
// RUN:   -Xcc %S/../Inputs/unextended-module-overlay.yaml \
// RUN:   -Xcc -DDUMMY \
// RUN:   -Xcc -I%S/Inputs/test.hmap \
// RUN:   -Xcc -I -Xcc %S/Inputs/test.hmap \
// RUN:   -Xcc -iquote -Xcc %S/Inputs/test.hmap
// RUN: %target-swift-frontend %s -typecheck -I %t

// Check the actual serialized search paths.
// RUN: llvm-bcanalyzer -dump %t/has_xref.swiftmodule > %t/has_xref.swiftmodule.txt
// RUN: %FileCheck %s < %t/has_xref.swiftmodule.txt
// RUN: %FileCheck -check-prefix=NEGATIVE %s < %t/has_xref.swiftmodule.txt

import has_xref

numeric(42)

// CHECK-LABEL: <OPTIONS_BLOCK
// CHECK: <XCC abbrevid={{[0-9]+}}/> blob data = '-working-directory'
// CHECK: <XCC abbrevid={{[0-9]+}}/> blob data = '{{.+}}/secret'
// CHECK: <XCC abbrevid={{[0-9]+}}/> blob data = '-DDUMMY'
// CHECK: </OPTIONS_BLOCK>

// CHECK-LABEL: <INPUT_BLOCK
// CHECK: <SEARCH_PATH abbrevid={{[0-9]+}} op0=1 op1=0/> blob data = '{{.+}}/secret/../Frameworks'
// CHECK: <SEARCH_PATH abbrevid={{[0-9]+}} op0=0 op1=0/> blob data = '{{.+}}/secret/.'
// CHECK: </INPUT_BLOCK>

// NEGATIVE-NOT: '.'
// NEGATIVE-NOT: '../Frameworks'
// This should be filtered out.
// NEGATIVE-NOT: -ivfsoverlay{{.*}}unextended-module-overlay.yaml
// NEGATIVE-NOT: .hmap
