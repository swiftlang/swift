// RUN: %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/zats (Sash Zats)
// Radar: http://openradar.appspot.com/22917580

class B {
}

class A: B {
    override init() {
        defer {
            super.init()
        }
    }
}
