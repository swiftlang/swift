// RUN: rm -rf %t && mkdir -p %t/secret
// RUN: %target-swift-frontend -emit-module -o %t/secret %S/Inputs/struct_with_operators.swift
// RUN: mkdir -p %t/Frameworks/has_alias.framework/Modules/has_alias.swiftmodule/
// RUN: %target-swift-frontend -emit-module -o %t/Frameworks/has_alias.framework/Modules/has_alias.swiftmodule/%target-swiftmodule-name %S/Inputs/alias.swift -module-name has_alias

// RUN: cd %t/secret && %target-swiftc_driver -emit-module -o %t/has_xref.swiftmodule -I . -F ../Frameworks -parse-as-library %S/Inputs/has_xref.swift %S/../Inputs/empty.swift -Xfrontend -serialize-debugging-options -Xcc -ivfsoverlay -Xcc %S/../Inputs/unextended-module-overlay.yaml -Xcc -DDUMMY
// RUN: %target-swift-frontend %s -typecheck -I %t

// Check the actual serialized search paths.
// RUN: llvm-bcanalyzer -dump %t/has_xref.swiftmodule > %t/has_xref.swiftmodule.txt
// RUN: %FileCheck %s < %t/has_xref.swiftmodule.txt
// RUN: %FileCheck -check-prefix=NEGATIVE %s < %t/has_xref.swiftmodule.txt

// XFAIL: linux

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
