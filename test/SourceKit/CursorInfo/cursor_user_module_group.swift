// RUN: %empty-directory(%t.mod)
// RUN: %target-swift-frontend -module-name MyModule -emit-module -emit-module-path %t.mod/MyModule.swiftmodule  -emit-module-doc -emit-module-doc-path %t.mod/MyModule.swiftdoc -group-info-path %S/Inputs/group.json %S/Inputs/FooSwiftModule.swift

// RUN: %sourcekitd-test -req=cursor -pos=7:10 %s -- %s -I %t.mod -target %target-triple | %FileCheck -check-prefix=CHECK %s

import MyModule
_ = fooSwiftFunc()

// CHECK: <Group>GroupA</Group>
