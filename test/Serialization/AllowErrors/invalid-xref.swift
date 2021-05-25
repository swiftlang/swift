// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/mods)

// RUN: touch %t/empty.swift
// RUN: %{python} %utils/split_file.py -o %t %s

// We're going to swap A and B around to cause an invalid xref
// RUN: %target-swift-frontend -emit-module -o %t/mods/A.swiftmodule -module-name A %t/lib.swift
// RUN: %target-swift-frontend -emit-module -o %t/mods/B.swiftmodule -module-name B %t/empty.swift

// Compile using SomeType from A
// RUN: %target-swift-frontend -module-name errors -emit-module -o %t/mods/errorsmain.partial.swiftmodule -I %t/mods %t/errors.swift
// Empty module so we can do a merge modules step
// RUN: %target-swift-frontend -module-name errors -emit-module -o %t/mods/errorsempty.partial.swiftmodule %t/empty.swift

// Swap A and B
// RUN: %target-swift-frontend -emit-module -o %t/mods/A.swiftmodule -module-name A %t/empty.swift
// RUN: %target-swift-frontend -emit-module -o %t/mods/B.swiftmodule -module-name B %t/lib.swift

// RUN: %target-swift-frontend -module-name errors -emit-module -o %t/mods/errors.swiftmodule -experimental-allow-module-with-compiler-errors %t/mods/errorsmain.partial.swiftmodule %t/mods/errorsempty.partial.swiftmodule

// Expect this to crash without allowing errors (we should never get into a
// situation where merge modules is run with MissingMemberDecls)
// RUN: not --crash %target-swift-frontend -module-name errors -emit-module -o %t/mods/errors.swiftmodule %t/mods/errorsmain.partial.swiftmodule %t/mods/errorsempty.partial.swiftmodule

// Needed for the "crash" test.
// REQUIRES: asserts

// BEGIN lib.swift
public struct SomeType {
    public init() {}
}


// BEGIN errors.swift
import A
import B

public class SomeClass {
  public let member: SomeType
  public init(member: SomeType) {
    self.member = member
  }
}
