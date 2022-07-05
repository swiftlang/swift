// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(module1)) %S/Inputs/cross_module_derivative_attr/module1/module1.swift %S/Inputs/cross_module_derivative_attr/module1/module1_other_file.swift -emit-module -emit-module-path %t/module1.swiftmodule -module-name module1
// RUN: %target-build-swift -I%t -L%t %S/Inputs/cross_module_derivative_attr/main/main.swift -o %t/a.out -lmodule1 %target-rpath(%t)
// RUN: %target-codesign %t/a.out
// RUN: %target-codesign %t/%target-library-name(module1)
// RUN: %target-run %t/a.out %t/%target-library-name(module1)
// REQUIRES: executable_test
