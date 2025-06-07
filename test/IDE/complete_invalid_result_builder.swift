// RUN: %batch-code-completion

enum Either<T,U> { case first(T), second(U) }
indirect enum ResultBuilderTerm<Expression> {
    case expression(Expression)
    case block([ResultBuilderTerm])
    case either(Either<ResultBuilderTerm, ResultBuilderTerm>)
}

protocol ResultBuilder {
    associatedtype Expression
    typealias Component = ResultBuilderTerm<Expression>
    associatedtype FinalResult

    static func buildFinalResult(_ component: Component) -> FinalResult
}

extension ResultBuilder {
    static func buildExpression(_ expression: Expression) -> Component { .expression(expression) }
    static func buildBlock(_ components: Component...) -> Component { .block(components) }
    static func buildEither(first: Component) -> Component { .either(.first(first)) }
    static func buildEither(second: Component) -> Component { .either(.second(second)) }

}

@resultBuilder
enum ArrayBuilder<E>: ResultBuilder {
    typealias Expression = E
    typealias FinalResult = [E]

    static func buildFinalResult(_ component: Component) -> FinalResult {
        switch component {
        case .expression(let e): return [e]
        case .block(let children): return children.flatMap(buildFinalResult)
        case .either(.first(let child)): return buildFinalResult(child)
        case .either(.second(let child)): return buildFinalResult(child)
        }
    }
}

func test(@ArrayBuilder<Int> a: () -> [Int]) {}
enum MyEnum { case a, b } 

test {
  switch MyEnum.a {
  case .#^EMPTYCASE?check=MYENUM_MEMBERS^#
  }
}

test {
  switch MyEnum.a {
  case .a:
  case .#^SECONDEMPTYCASE?check=MYENUM_MEMBERS^#
  }
}

// MYENUM_MEMBERS-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: a[#MyEnum#]; name=a
// MYENUM_MEMBERS-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: b[#MyEnum#]; name=b
