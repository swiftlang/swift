// RUN: %target-swift-frontend -emit-sil %s -g -O -module-name a

// Make sure inlining a function with a move-only optional does not crash
// due to forwardToInit forcing owned ownership in debug reconstruction blocks.

// TODO: investigate why all debug values end up being undef.

struct TrivialMoveOnly: ~Copyable {
    var payload: Int
}

func someFunction() {
    let instance = TrivialMoveOnly(payload: 100)
    let container: TrivialMoveOnly? = instance

    if case .some(let boundValue) = container {
        let _ = boundValue.payload
    }
}

someFunction()
