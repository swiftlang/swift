// RUN: not --crash %target-swift-frontend %s -parse

extension RawRepresentable {
    init?(rawValue optionalRawValue: RawValue?) {
        guard let rawValue = optionalRawValue, value = Self(rawValue: rawValue) else { return nil }
        self = value
    }
}

enum E: Int {
    case A = 0, B
}

let v: E = .A
