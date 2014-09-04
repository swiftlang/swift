// RUN: not --crash %swift -emit-ir %s
// Test case submitted to project by https://github.com/practicalswift (practicalswift)

func ^(a: BooleanType, Bool) -> Bool {
    return !(a)
}
