// RUN: %target-typecheck-verify-swift -swift-version 6

// REQUIRES: concurrency

struct MyType<T: Sendable> : Sendable {
    enum MyError: Error {
        case error
    }

    public init(_ h: T) { self.handler = h }

    public var handler: T

    public func handle<OperationInput, OperationOutput>(
      forOperation operationID: String,
      using handlerMethod: @Sendable @escaping (T)
        -> ((OperationInput) async throws -> OperationOutput),
      deserializer: @Sendable @escaping (
      ) throws -> OperationInput,
      serializer: @Sendable @escaping (OperationOutput) throws
        -> ()
    ) async throws -> () where OperationInput: Sendable,
                               OperationOutput: Sendable
    {
        @Sendable
        func wrappingErrors<R>(
          work: () async throws -> R,
          mapError: (any Error) -> any Error
        ) async throws -> R {
            fatalError()
        }
        @Sendable
        func makeError(
          input: OperationInput? = nil,
          output: OperationOutput? = nil,
          error: any Error
        ) -> any Error {
            fatalError()
        }
        var next: @Sendable () async throws // expected-warning {{}}
          -> () = {
              let input: OperationInput = try await wrappingErrors {
                  try deserializer()
              } mapError: { error in
                  makeError(error: error)
              }
              let output: OperationOutput = try await wrappingErrors {
                  let method = handlerMethod(handler)
                  return try await wrappingErrors {
                      try await method(input)
                  } mapError: { error in
                      MyError.error
                  }
              } mapError: { error in
                  makeError(input: input, error: error)
              }
              return try await wrappingErrors {
                  try serializer(output)
              } mapError: { error in
                  makeError(input: input, output: output, error: error)
              }
          }
        return try await next()
    }
}
