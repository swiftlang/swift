// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-module -emit-library -module-name ModuleA -module-link-name ModuleA %S/A.swift -o %t/%target-library-name(ModuleA)
// RUN: %target-swift-frontend -c -I %t -L %t -lModuleA -parse-as-library %s 

import ModuleA

@main
extension Main {
}
