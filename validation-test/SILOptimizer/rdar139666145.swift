// RUN: %target-build-swift %s

struct Box<T> { var value: T }

func modify(_ string: inout String) {}

func tryConsume() {
    var box = Box(value: "")
    modify(&box.value)
    print(consume box)
}
