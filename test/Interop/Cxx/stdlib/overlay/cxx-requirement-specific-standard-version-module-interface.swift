// RUN: %target-swift-ide-test -print-module -module-to-print=CxxRequirementSpecificStandardVersion -source-filename=x -I %S/Inputs -cxx-interoperability-mode=default | %FileCheck %s

// CHECK: CxxConvertibleToBool
