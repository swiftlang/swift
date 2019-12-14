// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -I%t -c -parse-as-library -emit-module -module-name module1 -emit-module-path %t/module1.swiftmodule %S/Inputs/derivative_attr_type_checking/module1/module1.swift
// RUN: %target-swift-frontend -I%t -c -parse-as-library -emit-module -module-name module2 -emit-module-path %t/module2.swiftmodule %S/Inputs/derivative_attr_type_checking/module2/module2.swift
// RUN: %target-swift-frontend -typecheck -verify -I%t %S/Inputs/derivative_attr_type_checking/main/main.swift %S/Inputs/derivative_attr_type_checking/main/other_file.swift
