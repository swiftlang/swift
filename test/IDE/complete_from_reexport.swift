// RUN: %empty-directory(%t)
//
// RUN: %target-swift-frontend -emit-module -module-name FooSwiftModule %S/Inputs/foo_swift_module.swift -o %t
// RUN: %target-swift-frontend -emit-module -module-name FooSwiftModuleOverlay %S/Inputs/foo_swift_module_overlay.swift -I %t -o %t
//
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_1 -I %t > %t.txt
// RUN: %FileCheck %s -check-prefix=TOP_LEVEL_1 < %t.txt
// RUN: %FileCheck %s -check-prefix=NO_DUPLICATES < %t.txt

// TOP_LEVEL_1-DAG: Decl[FreeFunction]/OtherModule[FooSwiftModuleOverlay]:     overlaidFoo()[#Void#]{{; name=.+$}}
// TOP_LEVEL_1-DAG: Decl[FreeFunction]/OtherModule[FooSwiftModuleOverlay]:     onlyInFooOverlay()[#Void#]{{; name=.+$}}

// FIXME: there should be only one instance of this completion result.
// NO_DUPLICATES: overlaidFoo()[#Void#]{{; name=.+$}}
// NO_DUPLICATES: overlaidFoo()[#Void#]{{; name=.+$}}
// NO_DUPLICATES-NOT: overlaidFoo()[#Void#]{{; name=.+$}}

import FooSwiftModuleOverlay

#^TOP_LEVEL_1^#
