// {"signature":"swift::ProtocolConformanceRef::forAbstract(swift::Type, swift::ProtocolDecl*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: rdar152763265
@resultBuilder struct a {
  static buildBlock<b, c, d, e>(b, c, d, e) func f<h>(_ : Bool @a Bool->h) {                         f(true {
    cond in var g : Int g 2 30\ g
