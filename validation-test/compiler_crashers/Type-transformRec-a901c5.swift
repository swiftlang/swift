// {"kind":"typecheck","original":"22aaf43f","signature":"swift::Type::transformRec(llvm::function_ref<std::__1::optional<swift::Type> (swift::TypeBase*)>) const","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  associatedtype b
  associatedtype c
}
protocol d: a where e: a {
  associatedtype e
  protocol f: d where b == (e.b, c) {
    func i<each g>()
    where repeat each g: f, repeat (each g).e == each g, h == (repeat (each g.b))
  }
}
