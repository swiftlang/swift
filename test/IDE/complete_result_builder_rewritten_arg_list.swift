// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CC | %FileCheck %s

@resultBuilder
struct Builder {
  static func buildBlock<T>(_ components: T...) -> T {
    fatalError()
  }
}

enum E {
  case e
}

struct S {
  init(_ x: String) {}
  func foo(_ x: String? = nil, y: E) -> S {
    fatalError()
  }
}

struct R {
  @Builder
    var foo: S {
      // The argument list for foo gets rewritten to remove the DefaultArgumentExpr
      // when doing a fallback type-check. Make sure we can continue to correctly
      // recover the MemberRefExpr from DeclRefExpr for .e
      S(E.#^CC^#).foo(y: .e)
      // CHECK: Decl[EnumElement]/CurrNominal: e[#E#]; name=e
    }
}

