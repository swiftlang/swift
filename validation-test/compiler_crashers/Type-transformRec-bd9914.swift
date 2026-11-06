// {"kind":"typecheck","original":"16a4bb3d","signature":"swift::Type::transformRec(llvm::function_ref<std::__1::optional<swift::Type> (swift::TypeBase*)>) const","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype A
  associatedtype b where c == Self
  protocol d: a where A == [b] {
    extension a
    where
      A == b,
      Self: d
    {
      e->b
    }
  }
}
