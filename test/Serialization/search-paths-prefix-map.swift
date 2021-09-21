// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Frameworks/has_alias.framework/Modules/has_alias.swiftmodule)
// RUN: %target-swift-frontend -emit-module -o %t/Frameworks/has_alias.framework/Modules/has_alias.swiftmodule/%target-swiftmodule-name %S/Inputs/alias.swift -module-name has_alias
// RUN: %empty-directory(%t/secret)
// RUN: %target-swift-frontend -emit-module -o %t/secret %S/Inputs/struct_with_operators.swift
// RUN: %target-swift-frontend -emit-module -o %t -I %t/secret -F %t/Frameworks -Fsystem %t/SystemFrameworks %S/Inputs/has_xref.swift
// RUN: %empty-directory(%t/workingdir)
// RUN: cd %t/workingdir && %target-swift-frontend -sdk %t/sdk %s -emit-module -o %t/prefixed.swiftmodule \
// RUN:   -I %t -I %t/secret -F %t/Frameworks -Fsystem %t/SystemFrameworks \
// RUN:   -Xcc -I -Xcc %t/include -Xcc -isystem -Xcc %t/system -Xcc -F -Xcc %t/fw \
// RUN:   -Xcc -I%t/includejoined -Xcc -isystem%t/systemjoined -Xcc -F%t/fwjoined \
// RUN:   -Xcc -D -Xcc donotprefixme -prefix-serialized-debugging-options \
// RUN:   -debug-prefix-map %t/workingdir=WORKINGDIR -debug-prefix-map %t/sdk=SDKROOT -debug-prefix-map %t=SRC -debug-prefix-map donotprefixme=ERROR
// RUN: llvm-bcanalyzer -dump %t/prefixed.swiftmodule | %FileCheck %s

import has_xref

numeric(42)

// CHECK-LABEL: <OPTIONS_BLOCK
// CHECK: <SDK_PATH abbrevid={{[0-9]+}}/> blob data = 'SDKROOT'
// CHECK: <XCC abbrevid={{[0-9]+}}/> blob data = '-working-directory'
// CHECK: <XCC abbrevid={{[0-9]+}}/> blob data = 'WORKINGDIR'
// CHECK: <XCC abbrevid={{[0-9]+}}/> blob data = '-I'
// CHECK: <XCC abbrevid={{[0-9]+}}/> blob data = 'SRC/include'
// CHECK: <XCC abbrevid={{[0-9]+}}/> blob data = '-isystem'
// CHECK: <XCC abbrevid={{[0-9]+}}/> blob data = 'SRC/system'
// CHECK: <XCC abbrevid={{[0-9]+}}/> blob data = '-F'
// CHECK: <XCC abbrevid={{[0-9]+}}/> blob data = 'SRC/fw'
// CHECK: <XCC abbrevid={{[0-9]+}}/> blob data = '-ISRC/includejoined'
// CHECK: <XCC abbrevid={{[0-9]+}}/> blob data = '-isystemSRC/systemjoined'
// CHECK: <XCC abbrevid={{[0-9]+}}/> blob data = '-FSRC/fwjoined'
// CHECK: <XCC abbrevid={{[0-9]+}}/> blob data = '-D'
// CHECK: <XCC abbrevid={{[0-9]+}}/> blob data = 'donotprefixme'
// CHECK-NOT: <XCC abbrevid={{[0-9]+}}/> blob data = '-fdebug-prefix-map
// CHECK: </OPTIONS_BLOCK>

// CHECK-LABEL: <INPUT_BLOCK
// CHECK: <SEARCH_PATH abbrevid={{[0-9]+}} op0=1 op1=0/> blob data = 'SRC/Frameworks'
// CHECK: <SEARCH_PATH abbrevid={{[0-9]+}} op0=1 op1=1/> blob data = 'SRC/SystemFrameworks'
// CHECK: <SEARCH_PATH abbrevid={{[0-9]+}} op0=0 op1=0/> blob data = 'SRC'
// CHECK: <SEARCH_PATH abbrevid={{[0-9]+}} op0=0 op1=0/> blob data = 'SRC/secret'
// CHECK: </INPUT_BLOCK>

// RUN: cd %t/workingdir && %target-swift-frontend -sdk %t/sdk %s -emit-module -o %t/unprefixed.swiftmodule \
// RUN:   -I %t -F %t/Frameworks \
// RUN:   -Xcc -I -Xcc %t/include \
// RUN:   -debug-prefix-map %t=TESTPREFIX
// RUN: llvm-bcanalyzer -dump %t/unprefixed.swiftmodule | %FileCheck --check-prefix=UNPREFIXED %s

// UNPREFIXED-NOT: TESTPREFIX