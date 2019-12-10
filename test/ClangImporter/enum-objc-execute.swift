// RUN: %empty-directory(%t)
// RUN: %target-build-swift -import-objc-header %S/Inputs/enum-objc.h %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: OS=macosx

import Foundation

// Test rawValue inits for non-frozen + frozen enums.

// CHECK1-NEXT: SwiftEnum
print(SwiftEnum.init(rawValue: 0)) 
// CHECK1-NEXT: SwiftEnum
print(SwiftEnum.init(rawValue: 1))
// CHECK1-NEXT: SwiftEnum
print(SwiftEnum.init(rawValue: 2))
// CHECK1-NEXT: SwiftEnum
print(SwiftEnum.init(rawValue: 3))
// CHECK1-NEXT: SwiftEnum
print(SwiftEnum.init(rawValue: 4))
// CHECK1-NEXT: SwiftEnum
print(ExhaustiveEnum.init(rawValue: 999))

// CHECK1-NEXT: nil
print(ExhaustiveEnum.init(rawValue: 0)) 
// CHECK1-NEXT: Optional(ExhaustiveEnum)
print(ExhaustiveEnum.init(rawValue: 1))
// CHECK1-NEXT: Optional(ExhaustiveEnum)
print(ExhaustiveEnum.init(rawValue: 2))
// CHECK1-NEXT: Optional(ExhaustiveEnum)
print(ExhaustiveEnum.init(rawValue: 3))
// CHECK1-NEXT: nil
print(ExhaustiveEnum.init(rawValue: 4))
// CHECK1-NEXT: nil
print(ExhaustiveEnum.init(rawValue: 999))

// Test rawValue getters after an init for non-frozen + frozen enums.

// CHECK1-NEXT: Optional(0)
print(SwiftEnum.init(rawValue: 0)?.rawValue) 
// CHECK1-NEXT: Optional(1)
print(SwiftEnum.init(rawValue: 1)?.rawValue)
// CHECK1-NEXT: Optional(2)
print(SwiftEnum.init(rawValue: 2)?.rawValue)
// CHECK1-NEXT: Optional(3)
print(SwiftEnum.init(rawValue: 3)?.rawValue)
// CHECK1-NEXT: Optional(4)
print(SwiftEnum.init(rawValue: 4)?.rawValue)
// CHECK1-NEXT: Optional(999)
print(SwiftEnum.init(rawValue: 999)?.rawValue)

// CHECK1-NEXT: nil
print(ExhaustiveEnum.init(rawValue: 0)?.rawValue) 
// CHECK1-NEXT: Optional(1)
print(ExhaustiveEnum.init(rawValue: 1)?.rawValue)
// CHECK1-NEXT: EOptional(2)
print(ExhaustiveEnum.init(rawValue: 2)?.rawValue)
// CHECK1-NEXT: Optional(3)
print(ExhaustiveEnum.init(rawValue: 3)?.rawValue)
// CHECK1-NEXT: nil
print(ExhaustiveEnum.init(rawValue: 4)?.rawValue)
// CHECK1-NEXT: nil
print(SwiftEnum.init(rawValue: 999)?.rawValue)