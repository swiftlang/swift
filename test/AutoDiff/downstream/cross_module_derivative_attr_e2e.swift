// RUN: %empty-directory(%t)
// RUN: %target-build-swift -I%t -parse-as-library -emit-module -module-name module1 -emit-module-path %t/module1.swiftmodule -emit-library -o %t/%target-library-name(module1) %S/Inputs/cross_module_derivative_attr_e2e/module1/module1.swift %S/Inputs/cross_module_derivative_attr_e2e/module1/module1_other_file.swift -Xllvm -enable-experimental-cross-file-derivative-registration -Xfrontend -validate-tbd-against-ir=none
// RUN: %target-build-swift -I%t -L%t %S/Inputs/cross_module_derivative_attr_e2e/main/main.swift -o %t/a.out -lm -lmodule1 -Xllvm -enable-experimental-cross-file-derivative-registration -Xfrontend -validate-tbd-against-ir=none %target-rpath(%t)
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

// TODO(TF-1104): Fix.
// XFAIL: swift_test_mode_optimize
