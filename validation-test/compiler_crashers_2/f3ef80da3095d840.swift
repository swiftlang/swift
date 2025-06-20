// {"signature":"swift::AutoClosureExpr::getUnwrappedCurryThunkExpr() const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a { associatedtype b associatedtype c where c == Dictionary<String, b> func d -> c? }
                  let e: a e.d
