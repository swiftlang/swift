// {"kind":"typecheck","original":"cc407c71","signature":"(anonymous namespace)::TypeSubstituter::transformOpaqueTypeArchetypeType(swift::OpaqueTypeArchetypeType*, swift::TypePosition)","stackOverflow":true}
// RUN: not %target-swift-frontend -typecheck %s
protocol a
  @resultBuilder struct b {
    static  buildBlock<each c>(repeat each c) -> d<repeat each c>
  }
  struct d<each e
    struct f<e {
      @b  content: e
    }
    extension f: a
      var body: some a {
        f {
          f {
            body
