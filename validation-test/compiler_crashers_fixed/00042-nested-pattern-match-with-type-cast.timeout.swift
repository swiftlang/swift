// RUN: %target-swift-frontend %s -emit-ir

// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/17970286

enum A {
    case A, B
}

enum B {
    case C(A, Any)
}

let x = B.C(A.A, "")
switch x {
    case .C(.A, let foo as String):
        print("")
    case .C(.B, let foo as String):
        print("")
    default:
        print("")
}
