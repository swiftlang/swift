// RUN: %target-swift-frontend %s -typecheck

// https://github.com/apple/swift/issues/66041

protocol Middleware<Input, Output, NextInput, NextOutput> {
    associatedtype Input
    associatedtype Output
    associatedtype NextInput
    associatedtype NextOutput
}

protocol Routes<Input, Output> {
    associatedtype Input
    associatedtype Output
}

extension Routes {
    func compose<M>(_ middleware: M) -> any Routes<M.NextInput, M.NextOutput> where
        M: Middleware,
        M.Input == Input,
        M.Output == Output
    {
        fatalError()
    }

    func mapInput<NewInput>(
        _ transform: @escaping (Input) -> NewInput
    ) -> any Routes<NewInput, Output> {
        fatalError()
    }
}

struct Request {}
struct Response {}
struct User {}

struct AuthMiddleware<Input, Output>: Middleware {
    typealias NextInput = (Input, login: User)
    typealias NextOutput = Output
}

func main(routes: any Routes<Request, Response>) {
    let routes = routes.compose(AuthMiddleware())
        .mapInput {
            (request: $0.0, login: $0.login)
        }
}
