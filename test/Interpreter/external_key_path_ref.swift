// RUN: %target-run-simple-swift
// REQUIRES: executable_test

struct Container<Base, Value, Content>
    where Base: Collection
{
    var base: Base
    var elementPath: KeyPath<Base.Element, Value>
    var content: (Value) -> Content
}

let x = Container(base: 1...10, elementPath: \.bitWidth) {
    return String($0, radix: 2)
} 

// CHECK: i hope we passed the audition
print("i hope we passed the audition")
