// RUN: %empty-directory(%t)
// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// CHECK: Start
print("Start")

struct Foo: ~Copyable {
  func idk() -> String {
    "idk"
  }

  deinit {
    print("foo")
  }
}

@available(SwiftStdlib 6.4, *)
func basic() {
  var intBox = Box(123)

  // CHECK: 123
  print(intBox[])

  intBox[] += 321

  // CHECK: 444
  print(intBox[])
}

@available(SwiftStdlib 6.4, *)
func noncopyable() {
  let fooBox = Box(Foo())

  // CHECK: bar
  print("bar")

  // CHECK: foo
}

@available(SwiftStdlib 6.4, *)
func consume() {
  func help() -> Foo {
    let fooBox = Box(Foo())

    let foo = fooBox.consume()

    // CHECK: bar
    print("bar")

    return foo
  }

  _ = help()

  // CHECK: foo
}

@available(SwiftStdlib 6.4, *)
func span() {
  let fooBox = Box(Foo())

  let fooSpan = fooBox.span

  // CHECK: 1
  print(fooSpan.count)

  // CHECK: idk
  print(fooSpan[0].idk())
}

@available(SwiftStdlib 6.4, *)
func mutableSpan() {
  var intBox = Box(123)

  let intSpan = intBox.mutableSpan

  // CHECK: 1
  print(intSpan.count)

  // CHECK: 123
  print(intSpan[0])
}

@available(SwiftStdlib 6.4, *)
func clone() {
  var intBox = Box(8)
  var cloneIntBox = intBox.clone()

  intBox[] += 8
  cloneIntBox[] -= 8

  // CHECK: 16
  print(intBox[])

  // CHECK: 0
  print(cloneIntBox[])
}

if #available(SwiftStdlib 6.4, *) {
  basic()
  noncopyable()
  consume()
  span()
  mutableSpan()
  clone()
}
