// RUN: not --crash %swift %s -emit-ir
// Test case submitted to project by https://github.com/owensd (David Owens II)

func a<T>() {
    enum b {
        case c
    }
}
