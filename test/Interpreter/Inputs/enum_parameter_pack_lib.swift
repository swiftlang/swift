// Library for enum_parameter_pack_cross_module test.
// This must be compiled separately to trigger the cross-module metadata path.

import Foundation

public enum Action<each T: Sendable>: Sendable {
    case stop(String)
    case record(RecordAction)
    case select(SelectAction)

    // Uses Foundation types inside the enum - this is important because
    // the external types trigger a specific metadata completion path.
    public struct RecordAction: Sendable {
        public let date: Date
        public let uuid: UUID
        public init(date: Date, uuid: UUID) {
            self.date = date
            self.uuid = uuid
        }
    }

    // Has pack expansion tuple - required to trigger the bug.
    public struct SelectAction: @unchecked Sendable {
        public let input: (repeat each T)
    }
}
