// RUN: %target-swift-emit-silgen -strict-concurrency=complete %s

protocol ServerStream {}
protocol Message {}

struct RPCServerHandlerContext {}
struct RPCRequestStream<Request: Sendable> {}
struct RPCResponseStream<Response: Sendable> {}

final class RPCServerHandler<Stream: ServerStream, Request: Message, Response: Message> {
    /// The actual user function. We are using a bi-directional function shape here and will map the other shapes into this one.
    private let userFunction: (
        RPCServerHandlerContext,
        RPCRequestStream<Request>,
        RPCResponseStream<Response>
    ) async throws -> Void

    init(
        userFunction: @escaping @Sendable (
            RPCServerHandlerContext,
            RPCRequestStream<Request>,
            RPCResponseStream<Response>
        ) async throws -> Void
    ) {
        self.userFunction = userFunction
    }
}
