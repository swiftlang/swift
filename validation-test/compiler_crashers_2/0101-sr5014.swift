// RUN: not --crash %target-swift-frontend -emit-ir -primary-file %s

// REQUIRES: asserts

struct Version {
}

extension CountableRange where Bound == Version {
    func contains(_ element: Version) -> Bool {
        fatalError()
    }
}
