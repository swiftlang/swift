// RUN: %target-swift-frontend -emit-silgen %s
protocol a {
  associatedtype b
  associatedtype c: AsyncSequence
  func d(e: b) -> c
}
protocol f {
  associatedtype n: a
}
func g<h, i: f>(
  j: h,
  k: i,
  o: h.b
) async where i.n == h {
  let l = j.d(e: o)
  try! await l.first(where: { m in
    true
  })
}
