// RUN: not --crash %target-swift-frontend %s -parse

// ASAN Output: stack-overflow on address 0x7fff31bf3ff8 (pc 0x0000022f8f44 bp 0x7fff31bf49d0 sp 0x7fff31bf4000 T0)

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
