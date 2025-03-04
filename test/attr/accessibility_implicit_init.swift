// RUN: %target-typecheck-verify-swift

public struct Number: RawRepresentable {
   // expected-error@-1 {{Implicit memberwise initializer 'init(rawValue:)' is declared internal because it was synthesized. This conflicts with the requirement that it be declared public because it matches a requirement in public protocol 'RawRepresentable'}} {{none}}
   // expected-note@-2 {{Explicitly declare initializer as 'public' to satisfy the requirement}} {{41-41=\n    public init(rawValue: Int) {\n        self.rawValue = rawValue\n    \}\n    }}
   public let rawValue: Int
}
