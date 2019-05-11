// RUN: %empty-directory(%t.mod)
// RUN: %target-swift-frontend -module-name MyModule -emit-module -emit-module-path %t.mod/MyModule.swiftmodule  -emit-module-doc -emit-module-doc-path %t.mod/MyModule.swiftdoc -group-info-path %S/Inputs/group.json %S/Inputs/swift_mod.swift %S/Inputs/swift_mod_syn.swift
// RUN: %sourcekitd-test -req=interface-gen -module MyModule -group-name A -- -I %t.mod -target %target-triple | %FileCheck -check-prefix=GROUPA %s
// RUN: %sourcekitd-test -req=interface-gen -module MyModule -group-name B -- -I %t.mod -target %target-triple | %FileCheck -check-prefix=GROUPB %s

// GROUPA: MyClass
// GROUPA-NOT: P1
// GROUPA-NOT: P2

// GROUPB: P1
// GROUPB: P2
// GROUPB-NOT: MyClass
