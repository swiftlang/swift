// RUN: not %target-swift-frontend -typecheck %s

protocol Box<Content> {
  associatedtype Content
  var value: Content {get set}
}
extension Box where Self == _Box {
  init(_ v: Content) {
    self.init(value: v)
  }
}

struct _Box<Content> {
  var value: Content
  init(value: Content) {
    self.value = value
  }
}
