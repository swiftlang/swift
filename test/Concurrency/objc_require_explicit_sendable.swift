// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/custom-modules %s -verify -parse-as-library -require-explicit-sendable

// REQUIRES: objc_interop
// REQUIRES: concurrency
import Foundation
import ObjCConcurrency

open class X: NXView { }
open class Y: X { }


open class Z: NSObject { } // expected-warning{{public class 'Z' does not specify whether it is 'Sendable' or not}}
// expected-note@-1{{add '@unchecked Sendable' conformance to class 'Z' if this type manually implements concurrency safety}}
// expected-note@-2{{make class 'Z' explicitly non-Sendable to suppress this warning}}
