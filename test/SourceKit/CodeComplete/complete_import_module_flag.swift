// RUN: %empty-directory(%t)
// RUN: %swift -Xcc -I%S/Inputs -emit-module -o %t/auxiliary_file.swiftmodule %S/Inputs/auxiliary_file.swift
// RUN: %complete-test -group=none -hide-none -raw -tok=TOP_LEVEL_0 %s -- -Xfrontend -import-module -Xfrontend auxiliary_file  -I %t -I %S/Inputs | %FileCheck %s
// RUN: %complete-test -group=none -tok=TOP_LEVEL_0 %s -- -Xfrontend -import-module -Xfrontend auxiliary_file  -I %t -I %S/Inputs | %FileCheck %s -check-prefix=WITH_HIDING

func fromMainModule() {}
func test() {
  #^TOP_LEVEL_0^#
}
// Score for fromAuxFile() ties fromMainModule(), so it goes first
// alphabetically.
// CHECK-LABEL: key.name: "fromAuxFile()
// CHECK-NOT: context
// CHECK: key.context: source.codecompletion.context.othermodule
// CHECK-NEXT: key.moduleimportdepth: 0

// CHECK-LABEL: key.name: "fromMainModule()
// CHECK-NOT: context
// CHECK: key.context: source.codecompletion.context.thismodule

// CHECK-LABEL: key.name: "fromImportedByAuxFile()
// CHECK-NOT: context
// CHECK: key.context: source.codecompletion.context.othermodule
// CHECK-NEXT: key.moduleimportdepth: 1

// WITH_HIDING: fromAuxFile()
// WITH_HIDING: fromMainModule()
// WITH_HIDING-NOT: fromImportedByAuxFile
