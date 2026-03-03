// RUN: %target-swift-frontend -emit-ir %s -disable-availability-checking

protocol ContextDescriptor: Sendable { }

protocol OutcomeDescriptor: Sendable { }

protocol ResultDescriptor: Sendable { }

protocol ErasedReducer<Context>: Sendable {
    associatedtype Context: ContextDescriptor
    associatedtype TriggerOutcome: OutcomeDescriptor
    associatedtype Result: ResultDescriptor

    var name: String { get }
    var function: @Sendable (_ context: Context?, _ trigger: TriggerOutcome) -> Result  { get }
}

protocol Reducer<Context, TriggerOutcome>: ErasedReducer {
    var name: String { get }
    var function: @Sendable (_ context: Context?, _ trigger: TriggerOutcome) -> Result  { get }
}

struct ExampleContext: ContextDescriptor { }

struct ExampleOutcome: OutcomeDescriptor { }

struct ExampleResult: ResultDescriptor { }

struct ExampleReducer<Context: ContextDescriptor, TriggerOutcome: OutcomeDescriptor, Result: ResultDescriptor>: Reducer {
    let name: String
    let function: @Sendable (_ context: Context?, _ trigger: TriggerOutcome) -> Result
}

class ExampleService<Context: ContextDescriptor> {
    let reducers:  [any ErasedReducer<Context>]

    public init(reducers: [any ErasedReducer<Context>]) {
        self.reducers = reducers
    }

    func reduce<TriggerOutcome: OutcomeDescriptor>(outcome: TriggerOutcome, context: Context) -> (any ResultDescriptor)? {
        // This line appears to be what's causing the crash
        guard let reducer = (reducers.compactMap { reducer in reducer as? any Reducer<Context, TriggerOutcome> }).first else {
            return nil
        }
        return reducer.function(context, outcome)
    }
}


public func testReducing() {
    let erasedReducers: [any ErasedReducer<ExampleContext>] = [
        ExampleReducer<ExampleContext, ExampleOutcome, ExampleResult>(name: "Example", function: { (_, _) in ExampleResult() })
    ]
    let context = ExampleContext()
    let trigger = ExampleOutcome()

    let service = ExampleService<ExampleContext>(reducers: erasedReducers)

    _ = service.reduce(outcome: trigger, context: context)
}
