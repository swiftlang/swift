// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/sr15807_a.swiftmodule %S/Inputs/sr15807_a.swift
// RUN: %target-swift-frontend -I %t -emit-sil -verify %s

import Foundation
import sr15807_a

struct ModuleBFoo: Codable, DefaultsSerializable {
}
