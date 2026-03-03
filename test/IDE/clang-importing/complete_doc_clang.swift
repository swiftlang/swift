// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=TOP -code-completion-comments -code-completion-sort-by-name \
// RUN:    -import-objc-header %S/Inputs/bridge.h -I %S/Inputs/somemod1 -I %S/Inputs/somemod2 | %FileCheck %s -check-prefix=CHECK-TOP
// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -code-completion -source-filename %s -code-completion-token=TOP -code-completion-comments -code-completion-sort-by-name \
// RUN:    -import-objc-header %S/Inputs/bridge.h -pch-output-dir %t.pch -I %S/Inputs/somemod1 -I %S/Inputs/somemod2 | %FileCheck %s -check-prefix=CHECK-TOP

// REQUIRES: objc_interop

import somemod2

#^TOP^#

// CHECK-TOP-LABEL: name=some_func11();
// CHECK-TOP-SAME:  briefcomment=some_func11 is cool function.;
// CHECK-TOP-SAME:  xmlcomment=<Function file="{{.*}}" line="2" column="6"><Name>some_func11</Name><USR>c:@F@some_func11</USR><Declaration>func some_func11()</Declaration><Abstract><Para> some_func11 is cool function.</Para></Abstract></Function>;
// CHECK-TOP-SAME:  rawcomment=some_func11 is cool function.

// CHECK-TOP-LABEL: name=some_func12();
// CHECK-TOP-SAME:  briefcomment=some_func12 is cool function.;
// CHECK-TOP-SAME:  xmlcomment=<Function file="{{.*}}" line="5" column="6"><Name>some_func12</Name><USR>c:@F@some_func12</USR><Declaration>func some_func12()</Declaration><Abstract><Para> some_func12 is cool function.</Para></Abstract></Function>;
// CHECK-TOP-SAME:  rawcomment=some_func12 is cool function.

// CHECK-TOP-LABEL: name=some_func21();
// CHECK-TOP-SAME:  briefcomment=some_func21 is cool function.;
// CHECK-TOP-SAME:  xmlcomment=<Function file="{{.*}}" line="2" column="6"><Name>some_func21</Name><USR>c:@F@some_func21</USR><Declaration>func some_func21()</Declaration><Abstract><Para> some_func21 is cool function.</Para></Abstract></Function>;
// CHECK-TOP-SAME:  rawcomment=some_func21 is cool function.

// CHECK-TOP-LABEL: name=some_func22();
// CHECK-TOP-SAME:  briefcomment=some_func22 is cool function.;
// CHECK-TOP-SAME:  xmlcomment=<Function file="{{.*}}" line="5" column="6"><Name>some_func22</Name><USR>c:@F@some_func22</USR><Declaration>func some_func22()</Declaration><Abstract><Para> some_func22 is cool function.</Para></Abstract></Function>;
// CHECK-TOP-SAME:  rawcomment=some_func22 is cool function.
