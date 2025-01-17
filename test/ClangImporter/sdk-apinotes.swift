// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -verify

import cfuncs

let array: [UnsafePointer<CChar>?] = [nil]
array.withUnsafeBufferPointer { nullability_note($0.baseAddress!) }
