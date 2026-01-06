// RUN: %empty-directory(%t.mod)
// RUN: %target-swift-frontend -module-name MyModule -emit-module -emit-module-path %t.mod/MyModule.swiftmodule  -emit-module-doc -emit-module-doc-path %t.mod/MyModule.swiftdoc -group-info-path %S/Inputs/group.json %S/Inputs/swift_mod.swift %S/Inputs/swift_mod_syn.swift
// RUN: %sourcekitd-test -req=interface-gen -module MyModule -group-name A -- -I %t.mod -target %target-triple | %FileCheck -check-prefix=GROUPA %s
// RUN: %sourcekitd-test -req=interface-gen -module MyModule -group-name B -- -I %t.mod -target %target-triple | %FileCheck -check-prefix=GROUPB %s

// FIXME: We don't currently handle group info for multi-file builds,
// so just make sure we don't crash.
// RUN: %empty-directory(%t.multifrontend)
// RUN: %target-build-swift -module-name MyModule -emit-module -emit-module-path %t.multifrontend/MyModule.swiftmodule -no-emit-module-separately -Xfrontend -group-info-path -Xfrontend %S/Inputs/group.json %S/Inputs/swift_mod.swift %S/Inputs/swift_mod_syn.swift
// RUN: %sourcekitd-test -req=interface-gen -module MyModule -group-name A -- -I %t.multifrontend -target %target-triple | %FileCheck -check-prefix=EMPTY %s

// GROUPA: MyClass
// GROUPA-NOT: P1
// GROUPA-NOT: P2

// GROUPB: P1
// GROUPB: P2
// GROUPB-NOT: MyClass

// EMPTY-NOT: P1
// EMPTY-NOT: P2
// EMPTY-NOT: MyClass
