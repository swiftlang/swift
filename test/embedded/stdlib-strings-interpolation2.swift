// RUN: %target-run-simple-swift(-swift-version 5 -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

struct Event {
  var id: String
  var n: Int
}

extension Event: CustomStringConvertible {
  var description: String {
    return "id: \(id), n: \(n)"
  }
}

@main
struct Main {
  static func main() {
    var events: [Event] = []
    for i in 0 ..< 5 {
      events.append(Event(id: "event\(i)", n: i))
    }
    for event in events {
      print(event)
    }
  }
}

// CHECK: id: event0, n: 0
// CHECK: id: event1, n: 1
// CHECK: id: event2, n: 2
// CHECK: id: event3, n: 3
// CHECK: id: event4, n: 4
