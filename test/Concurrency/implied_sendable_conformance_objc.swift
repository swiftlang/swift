// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -o /dev/null -I %S/Inputs/custom-modules %s -verify -parse-as-library -swift-version 6

// REQUIRES: objc_interop
// REQUIRES: concurrency

import Foundation

extension CGRect: Sendable {}
// expected-warning@-1 {{extension declares a conformance of imported type 'CGRect' to imported protocol 'Sendable'; this will not behave correctly if the owners of 'CoreGraphics' introduce this conformance in the future}}
// expected-note@-2 {{add '@retroactive' to silence this warning}}
// expected-error@-3 {{conformance to 'Sendable' must occur in the same source file as struct 'CGRect'; use '@unchecked Sendable' for retroactive conformance}}
protocol P: Sendable {}

extension CGPoint: P {}

