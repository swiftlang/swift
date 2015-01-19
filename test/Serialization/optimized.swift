// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -O -emit-module -o %t %s

// At one point this triggered deserialization of enough of the stdlib to cause
// an assertion failure in serialization.

class Rdar17567391 {
  let data: [Int] = []
}
