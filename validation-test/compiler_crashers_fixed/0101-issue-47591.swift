// RUN: not %target-swift-frontend -emit-ir -primary-file %s

// https://github.com/apple/swift/issues/47591

struct Version {
}

extension CountableRange where Bound == Version {
    func contains(_ element: Version) -> Bool {
        fatalError()
    }
}
