// RUN: %target-typecheck-verify-swift

infix operator <&>

func <&><A, B> (_: A, _: (A) -> B) -> B { fatalError() }

extension Array {
    func traverse<B>(_ fn: (Element) -> B) -> [B] { fatalError() }
}

struct Item {}
struct Statement {}

@resultBuilder
struct Builder {
    static func buildExpression(_: Statement) -> Item {
        fatalError()
    }

    static func buildBlock() -> Item {
        fatalError()
    }

    static func buildBlock(_: Item) -> Item {
        fatalError()
    }

    static func buildArray(_: [Item]) -> Item {
        fatalError()
    }

    static func buildFinalResult(_: Item) -> Statement {
        fatalError()
    }
}

func build(@Builder _: () -> Statement) -> Statement {
    fatalError()
}

func broken(xs: [Int]) -> Statement {
    return xs.traverse { $0 } <&> { xs in
        build {
            // the type of xs is just [Int], but we need to get that sorted
            // out before we start on the result builder.
            for _ in xs {
            }
        }
    }
}
