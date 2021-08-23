// RUN: %target-typecheck-verify-swift -verify-ignore-unknown

// MARK: - Synthesized CodingKeys Enum

// Enums which get synthesized Codable implementations should have visible
// CodingKey enums during member type lookup.
enum SynthesizedEnum : Codable {
  case value

}

