// {"kind":"emit-silgen","original":"aa7aa63f","signature":"swift::Lowering::PackElementGenerator::PackElementGenerator(swift::Lowering::AbstractionPattern, swift::CanTypeWrapper<swift::PackType>)","signatureAssert":"Assertion failed: (origPackType.matchesPack(substPackType)), function PackElementGenerator","signatureNext":"Lowering::AbstractionPattern::forEachPackElement"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@available(SwiftStdlib 5.9, *)
@resultBuilder struct a {
  static func buildBlock<each b>(c: repeat each b) -> d<> {
  }
}
@available(SwiftStdlib 5.9, *)
struct d<each e> {
  init(@a _: () -> d) {
  }
}
@available(SwiftStdlib 5.9, *)
struct f {
  func h<g, b>(i: g) -> b {
    d {
    }
    0
  }
}
