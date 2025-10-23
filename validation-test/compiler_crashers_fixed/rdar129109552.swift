// RUN: not %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop

import Foundation.NSZone

@available(macOS 12, iOS 15, tvOS 15, watchOS 8, *)
@dynamicMemberLookup public struct AttributedSubstring : Sendable {
}

@available(macOS 12, iOS 15, tvOS 15, watchOS 8, *)
extension AttributedSubstring : AttributedStringProtocol {
}
