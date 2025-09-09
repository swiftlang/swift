# Overlapping accesses, but operation requires exclusive access

Swift requires exclusive access to a variable in order to modify that variable. An error is reported if a program attempts to access a variable while it is already in the process of being accessed via another name. These issues can often be resolved by making a local copy of a variable before modifying it.

In the following example, `count` is accessed for modification by passing it as an `inout` argument to `modifyTwice`. This `inout` access to `count` lasts for the entire duration of the `modifyTwice` function call. The exclusivity violation occurs because the `modifier` closure also accesses `count` via the name `$0` while the first access is in progress.

```swift
func modifyTwice(_ value: inout Int, by modifier: (inout Int) -> ()) {
  modifier(&value)
  modifier(&value)
}

func testCount() {
  var count = 1
  modifyTwice(&count) { $0 += count } // error: overlapping accesses to 'count', but modification requires exclusive access; consider copying to a local variable
  print(count)
}
```

This exclusivity violation can be fixed by introducing a local variable to track the value the `modifier` closure should add to its argument.

```swift
func modifyTwice(_ value: inout Int, by modifier: (inout Int) -> ()) {
  modifier(&value)
  modifier(&value)
}

func testCount() {
  var count = 1
  let incrementBy = count
  modifyTwice(&count) { $0 += incrementBy }
  print(count)
}
```

In this updated program, `count` and `$0` no longer refer to the same variable, so accessing them at the same time does not violate exclusivity.

Another common source of exclusivity violations occurs when a `mutating` method modifies `self`:

```swift
extension Array {
    mutating func append(removingFrom other: inout Array<Element>) {
        while !other.isEmpty {
            self.append(other.removeLast())
        }
    }
}

var numbers = [1, 2, 3]
numbers.append(removingFrom: &numbers) // error: overlapping accesses to 'x', but modification requires exclusive access; consider copying to a local variable
```

A `mutating` method accesses `self` for the duration of the method call, so in this example `numbers` has two conflicting long-lived accesses, one as `self` and one as an `inout` argument to the method. Again, a local variable can be introduced to eliminate the conflict:

```swift
var numbers = [1, 2, 3]
var toAppend = numbers
numbers.append(removingFrom: &toAppend)
```

Exclusivity checks play an important role in enforcing memory safety and enabling compiler optimizations. To learn more, see [Swift 5 Exclusivity Enforcement](https://www.swift.org/blog/swift-5-exclusivity/) on the Swift.org blog.