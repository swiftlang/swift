protocol DemoBuilderComponent {
    var built: Built { get }
}

enum Built {
    case string(String)
    case int(Int)
    indirect case debuggable(Self, debugInfoProvider: DSLDebugInfoProvider)
}

@resultBuilder
struct DemoBuilder {
    static func buildExpression<T: DemoBuilderComponent>(
        _ expression: T
    ) -> Built {
        expression.built
    }
    
    // buildDebuggable
    
    static func buildBlock(_ components: Built...) -> [Built] {
        components
    }
}

// MARK: - own conformance -

extension String: DemoBuilderComponent {
    var built: Built {
        .string(self)
    }
}

extension Int: DemoBuilderComponent {
    var built: Built {
        .int(self)
    }
    
    func decorated() -> some DemoBuilderComponent {
        "==\(self)=="
    }
}

// MARK: - 3rd party conformance?
// Let's say... someone think we should build something else? Similar to custom SwiftUI views
extension Double: DemoBuilderComponent {
    var built: Built {
        .string(String(self))
    }
}

// MARK: - test -

func make(@DemoBuilder _ build: () -> [Built]) -> [Built] {
    build()
}

func use(_ built: [Built]) {
    class Context: CustomStringConvertible {
        var counter: Int = 0
        
        var description: String {
            "DEBUG CONTEXT: counter is \(counter)"
        }
    }
    let context = Context()
    for component in built {
        switch component {
        case .debuggable(let built, debugCallback: let supplyContextToDebugger):
            // FIXME: make this a no-op if not Onone
            supplyContextToDebugger(context)
            
            switch built {
            case .debuggable:
                fatalError("WRAPPED TOO MUCH")
            case .int(let int):
                print("INT: \(int)")
            case .string(let string):
                print("STRING: \(string)")
            }
        case .int(let int):
            print("Int \(int)")
        case .string(let string):
            print("String \(string)")
        }
        context.counter += 1
    }
}

let made = make {
    1
    "Hello"
    2.3
    4.decorated()
}

use(made)
