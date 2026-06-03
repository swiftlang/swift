// {"kind":"emit-silgen","original":"7749346b","signature":"(anonymous namespace)::Traversal::doIt(swift::Type)","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
struct a<b> {
  var c: [a<a>]
}
