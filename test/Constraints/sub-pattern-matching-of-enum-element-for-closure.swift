// RUN: %target-typecheck-verify-swift
enum TestType {
    case foo
    case bar(Bool, (a: String, String))
}

func test(type: TestType) -> String {
    let str: String = {
        switch type {
        case .foo:
            return ""
        case .bar(_, (let a, _)):
            return a
        }
    }()
    
    return str
}
