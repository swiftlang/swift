// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -I%t -c -parse-as-library -emit-module -module-name module1 -emit-module-path %t/module1.swiftmodule -o %t/module1.o %S/Inputs/derivative_registration/module1/module1.swift %S/Inputs/derivative_registration/module1/module1_other_file.swift
// RUN: %target-swift-frontend -I%t -c -parse-as-library -emit-module -module-name module2 -emit-module-path %t/module2.swiftmodule -o %t/module2.o %S/Inputs/derivative_registration/module2/module2.swift
// RUN: %target-build-swift -I%t %S/Inputs/derivative_registration/main/main.swift %S/Inputs/derivative_registration/main/other_file.swift %t/module1.o %t/module2.o -o %t/a.out -lm
// RUN: %target-run %t/a.out
// REQUIRES: executable_test
