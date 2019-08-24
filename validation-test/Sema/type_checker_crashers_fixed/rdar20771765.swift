// RUN: %target-swift-frontend %s -typecheck

enum Pattern {
case Specific(title: String?, label: String?, value: AnyObject?)
case General(String)
}

extension Pattern : CustomDebugStringConvertible {
  var debugDescription: String {
    switch(self) {
    case let .Specific(title, label, value):
      let elements = [
        title.map { "title: \(String(reflecting: $0))" },
        label.map { "label: \(String(reflecting: $0))" },
        value.map { "value: \(String(reflecting: $0))" }
      ].flatMap { $0 }
      return "x"
    case let .General(s):
      return ".General(\(String(reflecting: s))))"
    }
  }
}
