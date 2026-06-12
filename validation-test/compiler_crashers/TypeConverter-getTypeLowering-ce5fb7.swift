// {"kind":"emit-silgen","original":"55a94e74","signature":"swift::Lowering::TypeConverter::getTypeLowering(swift::Lowering::AbstractionPattern, swift::Type, swift::TypeExpansionContext)","signatureAssert":"Assertion failed: (hasNoNontrivialLexicalLeaf && \"Found non-trivial lexical leaf in non-trivial non-lexical type?!\"), function verifyLexicalLowering","signatureNext":"SILFunction::getTypeLowering"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
@resultBuilder
struct a {
  static func buildPartialBlock<c>(first: c) -> c {
  }
  static func buildPartialBlock<c, d>(accumulated: c, next: d) {
  }
  static func buildOptional<b>(_: b) -> b {
  }
}
protocol e {
  associatedtype f
  @a var l: f {
    get
  }
}
struct g<b: e> {
  let h: b.f
  init(_: b) {
  }
}
struct i: e {
  var k = 0
  var l: some Any {
    if k > 0 {
      ""
    }
  }
}
func j() {
  let m = g(i())
}
