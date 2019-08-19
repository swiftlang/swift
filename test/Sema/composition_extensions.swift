// XFAIL: *
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -primary-file %s %S/Inputs/composition_extensions_usage.swift -emit-module-path %t/P-partial.swiftmodule -module-name SR11227 -enable-testing
// RUN: %target-swift-frontend -primary-file %S/Inputs/composition_extensions_usage.swift %s -emit-module-path %t/S-partial.swiftmodule -module-name SR11227 -enable-testing
// RUN: %target-swift-frontend -sil-merge-partial-modules %t/P-partial.swiftmodule %t/S-partial.swiftmodule -emit-module -o %t/SR11227.swiftmodule

protocol P0 {}

protocol P1 {}

protocol P1_1 : P1 {
  func p1_1()
}

protocol P2 {}

extension P1 & P1_1 where Self : P2 {
  func p1_1() {}
}
