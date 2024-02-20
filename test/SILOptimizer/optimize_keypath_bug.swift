// RUN: %target-swift-frontend -emit-sil -O  -enable-ossa-modules -sil-verify-all %s
// REQUIRES: OS=macosx

import Foundation

// Check the following test does not crash with incorrect stack order
open class DateFormatter {
    typealias CFType = CFDateFormatter
    open func copy(with zone: NSZone? = nil) -> Any {
        let copied = DateFormatter()

        func __copy<T>(_ keyPath: ReferenceWritableKeyPath<DateFormatter, T>) {
            copied[keyPath: keyPath] = self[keyPath: keyPath]
        }

        __copy(\.formattingContext)
        return copied
    }

    public enum Context : Int {
        case unknown
        case dynamic
        case standalone
        case listItem
        case beginningOfSentence
        case middleOfSentence
    }
    open var formattingContext: Context = .unknown
}

