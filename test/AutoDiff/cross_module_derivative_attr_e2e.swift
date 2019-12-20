// RUN: %empty-directory(%t)
￼// RUN: %target-swift-frontend -I%t -c -parse-as-library -emit-module -module-name module1 -emit-module-path %t/module1.swiftmodule -o %t/module1.o %S/Inputs/cross_module_derivative_attr_e2e/module1/module1.swift %S/Inputs/cross_module_derivative_attr_e2e/module1/module1_other_file.swift -Xllvm -enable-experimental-cross-file-derivative-registration -validate-tbd-against-ir=none
￼// RUN: %target-build-swift -I%t %S/Inputs/cross_module_derivative_attr_e2e/main/main.swift %t/module1.o -o %t/a.out -lm -Xllvm -enable-experimental-cross-file-derivative-registration -Xfrontend -validate-tbd-against-ir=none
￼// RUN: %target-run %t/a.out
￼// REQUIRES: executable_test
