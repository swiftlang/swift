// {"kind":"emit-silgen","original":"b525eb89","signature":"swift::Type::findIf(llvm::function_ref<bool (swift::Type)>) const::Walker::walkToTypePre(swift::Type)","signatureAssert":"Assertion failed: (std::find(Sig.getGenericParams().begin(), Sig.getGenericParams().end(), gpt->getCanonicalType()) != Sig.getGenericParams().end() && \"generic param not declared in generic signature?!\"), function operator()","signatureNext":"Traversal::doIt"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
protocol a {
  associatedtype b
  var c: b {
    get
  }
}
protocol d {
  associatedtype e: a
}
protocol f {
  associatedtype g: d
  associatedtype l: BidirectionalCollection where l.Element == d
  func h(_: g.e.b) -> l
}
func i<j: f>(
  k: j,
  q: j.g.e
) {
  let m = k.h(q.c)
  var r = q
  for n in m {
    func o<p>(_: p) where p == j.g {
      r
    }
  }
}
