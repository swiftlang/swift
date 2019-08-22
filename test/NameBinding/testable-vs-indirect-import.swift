// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-testing -o %t/TestableVersusIndirect1.swiftmodule %S/Inputs/TestableVersusIndirect1.swift
// RUN: %target-swift-frontend -emit-module -enable-testing -o %t/TestableVersusIndirect2.swiftmodule %S/Inputs/TestableVersusIndirect2.swift
// RUN: %target-swift-frontend -emit-module -I %t -o %t/TestableVersusIndirectImporter.swiftmodule %S/Inputs/TestableVersusIndirectImporter.swift
// RUN: %target-swift-frontend -typecheck -I %t %s

@testable import TestableVersusIndirect1
import TestableVersusIndirectImporter
@testable import TestableVersusIndirect2

t1()
t2()
