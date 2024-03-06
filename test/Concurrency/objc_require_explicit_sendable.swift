// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -o /dev/null -I %S/Inputs/custom-modules %s -verify -parse-as-library -require-explicit-sendable
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -o /dev/null -I %S/Inputs/custom-modules %s -verify -parse-as-library -require-explicit-sendable -strict-concurrency=targeted
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -o /dev/null -I %S/Inputs/custom-modules %s -verify -parse-as-library -require-explicit-sendable -strict-concurrency=complete
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -o /dev/null -I %S/Inputs/custom-modules %s -verify -parse-as-library -require-explicit-sendable -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: objc_interop
// REQUIRES: concurrency
// REQUIRES: asserts

import Foundation
import ObjCConcurrency

open class X: NXView { }
open class Y: X { }


open class Z: NSObject { } // expected-warning{{public class 'Z' does not specify whether it is 'Sendable' or not}}
// expected-note@-1{{add '@unchecked Sendable' conformance to class 'Z' if this type manually implements concurrency safety}}
// expected-note@-2{{make class 'Z' explicitly non-Sendable to suppress this warning}}
