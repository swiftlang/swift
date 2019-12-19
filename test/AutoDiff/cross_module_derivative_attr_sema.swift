// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -I%t -c -parse-as-library -emit-module -module-name module1 -emit-module-path %t/module1.swiftmodule %S/Inputs/cross_module_derivative_attr_sema/module1/module1.swift
// RUN: %target-swift-frontend -I%t -c -parse-as-library -emit-module -module-name module2 -emit-module-path %t/module2.swiftmodule %S/Inputs/cross_module_derivative_attr_sema/module2/module2.swift
// RUN: %target-swift-frontend -typecheck -verify -I%t %S/Inputs/cross_module_derivative_attr_sema/main/main.swift %S/Inputs/cross_module_derivative_attr_sema/main/other_file.swift

