// RUN: %target-swift-frontend -emit-ir -target %target-cpu-apple-macosx10.15 %s
// REQUIRES: OS=macosx

protocol P {}

func one() -> some P {
    return two()
}

func two() -> some P {
    return three()
}

func three() -> some P {
    return one()
}
