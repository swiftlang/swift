// {"signature":"(anonymous namespace)::ExprRewriter::coerceToType(swift::Expr*, swift::Type, swift::constraints::ConstraintLocatorBuilder)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a < each b : Collection {
      c : (     repeat each b
        extension   
  struct Index {
          d : (repeat(each b.Index
  } var endIndex : Index {
                      Index(d:  repeat (each c).endIndex
