// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -g -enable-experimental-feature Embedded -c -o %t/main.o
// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// https://github.com/swiftlang/swift/issues/72627 - crash with noncopyable
// generic deinit.

final class Box<T: ~Copyable> {
  var value: T
  init(_ value: consuming T) {
   self.value = value
  }
}

struct ListNode<Element: ~Copyable>: ~Copyable {
    typealias Link = Box<ListNode<Element>>?

    var element: Element
    var next: Link
}

struct List<Element: ~Copyable>: ~Copyable {
  typealias Link = Box<ListNode<Element>>?
  
  var head: Link = nil

  init(head: consuming Link = nil) {
    self.head = head
  }

  mutating func push(_ element: consuming Element) {
    self = Self(head: Box(ListNode(element: element, next: self.head)))
  }
}

public func main() {
  var myList = List<Int>()
  myList.push(1)
  let _ = consume myList
}

