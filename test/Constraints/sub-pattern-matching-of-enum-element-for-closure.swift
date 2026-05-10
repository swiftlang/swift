// RUN: %target-typecheck-verify-swift
enum TestType {
    case foo
    case bar(Bool, (a: String, (b: String, (String, (c: String, Bool), String), String)))
}

func test(type: TestType) -> String {
    let str: String = {
        switch type {
        case .foo:
            return ""
        case .bar(_, (_, (_, (_, (let c, _), _), _))):
            return c
        }
    }()
    
    return str
}
