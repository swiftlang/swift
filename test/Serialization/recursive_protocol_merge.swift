// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -module-name rpm -o %t/a.swiftmodule -primary-file %S/Inputs/recursive_protocol_merge_a.swift %S/Inputs/recursive_protocol_merge_b.swift
// RUN: %target-swift-frontend -emit-module -module-name rpm -o %t/b.swiftmodule %S/Inputs/recursive_protocol_merge_a.swift -primary-file %S/Inputs/recursive_protocol_merge_b.swift
// RUN: %target-swift-frontend -merge-modules -emit-module -o %t/rpm.swiftmodule %t/a.swiftmodule %t/b.swiftmodule
