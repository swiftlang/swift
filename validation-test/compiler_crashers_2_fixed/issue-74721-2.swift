// RUN: %target-swift-frontend -emit-ir %s -disable-availability-checking

protocol Context: Sendable {}

protocol Service<ServiceContext> {
    associatedtype ServiceContext: Context
}

protocol Builder<BuilderContext> {
    associatedtype BuilderContext: Context

    init(context: BuilderContext?)

    func withContextReducer(
        reducer: @escaping @Sendable (_ context: BuilderContext?) async -> BuilderContext?
    ) -> Self


    func build() -> Result<any Service<BuilderContext>, Error>
}

final actor ServiceImplementation<ServiceContext: Context>: Service {
    var currentContext: ServiceContext?

    init(initialContext: ServiceContext?) {
        currentContext = initialContext
    }
}

final class BuilderImplementation<BuilderContext: Context>: Builder {
    var initialContext: BuilderContext? = nil

    init(context: BuilderContext?) {
        initialContext = context
    }

    func withContextReducer(
        reducer: @escaping @Sendable (_ context: BuilderContext?) async -> BuilderContext?
    ) -> Self {
        return self
    }

    func build() -> Result<any Service<BuilderContext>, Error> {
        return .success(
            ServiceImplementation(
                initialContext: initialContext
            )
        )
    }
}

enum BuilderFactory {
    // The generic parameter here makes the swift compiler crash
    static func create<T: Context>(context: T?) -> any Builder<T> {
        return BuilderImplementation(context: context)
    }
}


struct DemoContext: Context {
    let someValue: Int?

    public init(someValue: Int? = nil) {
        self.someValue = someValue
    }
}

public func testBuilding() {
        let builderResult: Result<any Service<DemoContext>, Error> = BuilderFactory.create(context: DemoContext())
            .withContextReducer(
                reducer: { (context: DemoContext?) -> DemoContext? in context }
            )
            .build()
    }

testBuilding()
