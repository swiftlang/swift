// RUN: %target-swift-frontend -verify -emit-ir -primary-file %s %S/Inputs/require-layout-error-other.swift -module-name layout

public func identity(_ s: S) -> S { return s }
