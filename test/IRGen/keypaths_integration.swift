// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path=%t/keypaths_integration_other.swiftmodule -module-name=keypaths_integration_other %S/Inputs/keypaths_integration_other.swift
// RUN: %target-swift-frontend -emit-ir -verify -I %t %s

import keypaths_integration_other

public func foo() -> KeyPath<Proto, Int> { return \.foo }
