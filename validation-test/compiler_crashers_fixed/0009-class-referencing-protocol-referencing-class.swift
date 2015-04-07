// RUN: %target-swift-frontend %s -emit-ir

// Test case submitted to project by https://github.com/practicalswift (practicalswift)

protocol b {
    var a: c<b> {
        get
    }
}

class c<d : b> {
}
