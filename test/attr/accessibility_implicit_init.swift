// RUN: %target-typecheck-verify-swift

public struct Number: RawRepresentable {
   // expected-error@-1 {{implicit memberwise initializer 'init(rawValue:)' is internal but must be declared public because it matches a requirement in public protocol 'RawRepresentable'}} {{none}}
   // expected-note@-2 {{explicitly declare the initializer as 'public' to satisfy the requirement}} {{41-41=\n    public init(rawValue: Int) {\n        self.rawValue = rawValue\n    \}\n    }}
   public let rawValue: Int
}
