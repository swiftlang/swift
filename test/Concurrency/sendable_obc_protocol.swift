// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -verify -parse-as-library

// REQUIRES: objc_interop
// REQUIRES: concurrency
import Foundation

@objc protocol P: Sendable { }
