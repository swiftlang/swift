// RUN: %empty-directory(%t)
// RUN: %target-build-swift -working-directory %t -I%t -parse-as-library -emit-module -module-name module1 -emit-module-path %t/module1.swiftmodule -emit-library -static %S/Inputs/cross_module_derivative_attr/module1/module1.swift %S/Inputs/cross_module_derivative_attr/module1/module1_other_file.swift -Xfrontend -validate-tbd-against-ir=none
// RUN: %target-build-swift -I%t -L%t %S/Inputs/cross_module_derivative_attr/main/main.swift -o %t/a.out -lm -lmodule1 -Xfrontend -validate-tbd-against-ir=none
// RUN: %target-run %t/a.out
// REQUIRES: executable_test
