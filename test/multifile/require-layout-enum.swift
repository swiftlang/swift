// RUN: %target-swift-frontend -emit-ir -primary-file %s %S/Inputs/require-layout-enum-other.swift -module-name layout

public func returnE() -> E { return .blah }
