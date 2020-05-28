// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -parse-as-library -emit-module -emit-module-path=%t/Module.swiftmodule -module-name=Module -DMODULE %s -O -emit-sil -o %t/module.sil
// RUN: %target-swift-frontend -module-name=main -DMAIN %s -I%t -O -emit-sil | %FileCheck %s

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

#if MODULE

public struct Foo: Equatable {
    @usableFromInline var optionA: Bool
    @usableFromInline var optionB: Optional<Int>

    public typealias ArrayLiteralElement = FooElement

    public struct FooElement: Equatable {
        @usableFromInline enum Backing: Equatable {
            case a
            case b(Int)
        }

        @usableFromInline var backing: Backing

        @inlinable internal init(backing: Backing) {
            self.backing = backing
        }

        public static let optionA = FooElement(backing: .a)

        @inlinable
        public static func getOptionA() -> FooElement {
                return FooElement(backing: .a)
        }
        @inlinable
        public static func optionB(_ x: Int) -> FooElement {
                return FooElement(backing: .b(x))
        }
    }
}

extension Foo: ExpressibleByArrayLiteral {
    @inlinable
    @inline(never)
    public init(arrayLiteral things: FooElement...) {
        self.optionA = false
        self.optionB = nil
        for thing in things {
            switch thing.backing {
                case .a:
                    self.optionA = true
                case .b(let x):
                    self.optionB = x
            }
        }
    }
}

#endif


#if MAIN

import Module

// Check if the array literal can be stack promoted.

// CHECK-LABEL: sil @$s4main6testit6Module3FooVyF
// CHECK:   alloc_ref [stack] [tail_elems $Foo.FooElement 
// CHECK: } // end sil function '$s4main6testit6Module3FooVyF'

public func testit() -> Foo {
    return [.optionA, .optionB(0xbeef), .optionA]
}


#endif


