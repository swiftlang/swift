extension NoRawTypeKey : CodingKey, UnsafeSendable {} // expected-warning{{'UnsafeSendable' is deprecated: Use @unchecked Sendable instead}}
extension StringKey : CodingKey, UnsafeSendable {} // expected-warning{{'UnsafeSendable' is deprecated: Use @unchecked Sendable instead}}
extension IntKey : CodingKey, UnsafeSendable {} // expected-warning{{'UnsafeSendable' is deprecated: Use @unchecked Sendable instead}}
