// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/OpaqueCrossFileB.swiftmodule -module-name OpaqueCrossFileB %S/Inputs/OpaqueCrossFileB.swift
// RUN: %target-swift-frontend -I %t -emit-ir -verify %s

import OpaqueCrossFileB

dump(anyFoo())
dump(anyFooProp)
dump(Subscript()[])

