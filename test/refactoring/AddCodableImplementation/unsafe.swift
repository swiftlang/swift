// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Implicit 'unsafe' in the synthesized bodies is written out only into files
// that check strict memory safety.

// RUN: %refactor -add-explicit-codable-implementation -source-filename %t/unchecked.swift -pos=1:16 | %FileCheck -check-prefix=UNCHECKED-UNSAFE %s
// RUN: %refactor -add-explicit-codable-implementation -source-filename %t/unchecked.swift -pos=5:8 | %FileCheck -check-prefix=UNCHECKED-SAFE %s
// RUN: %refactor -add-explicit-codable-implementation -source-filename %t/checked.swift -pos=3:16 | %FileCheck -check-prefix=CHECKED-UNSAFE %s
// RUN: %refactor -add-explicit-codable-implementation -source-filename %t/checked.swift -pos=7:8 | %FileCheck -check-prefix=CHECKED-SAFE %s

// UNCHECKED-UNSAFE: {{^ +}}self.value = try container.decode(Int.self, forKey: UnsafeType.CodingKeys.value)
// UNCHECKED-UNSAFE: {{^ +}}try container.encode(self.value, forKey: UnsafeType.CodingKeys.value)

// UNCHECKED-SAFE: {{^ +}}self.name = try container.decode(String.self, forKey: SafeType.CodingKeys.name)
// UNCHECKED-SAFE: {{^ +}}try container.encode(self.name, forKey: SafeType.CodingKeys.name)

// CHECKED-UNSAFE: {{^ +}}unsafe self.value = try container.decode(Int.self, forKey: UnsafeType.CodingKeys.value)
// CHECKED-UNSAFE: {{^ +}}unsafe try container.encode(self.value, forKey: UnsafeType.CodingKeys.value)

// FIXME: Nothing here is unsafe, so 'unsafe' is redundant.
// CHECKED-SAFE: {{^ +}}unsafe self.name = try container.decode(String.self, forKey: SafeType.CodingKeys.name)
// CHECKED-SAFE: {{^ +}}unsafe try container.encode(self.name, forKey: SafeType.CodingKeys.name)

//--- unchecked.swift
@unsafe struct UnsafeType: Codable {
  var value: Int
}

struct SafeType: Codable {
  var name: String
}

//--- checked.swift
default @diagnose(StrictMemorySafety, as: warning)

@unsafe struct UnsafeType: Codable {
  var value: Int
}

struct SafeType: Codable {
  var name: String
}
