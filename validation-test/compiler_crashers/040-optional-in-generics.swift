// RUN: not --crash %swift %s -emit-ir
// Test case submitted to project by https://github.com/tmu (Teemu Kurppa)
// rdar://18175202
func some<S: SequenceType, T where Optional<T> == S.Generator.Element>(xs : S) -> T? {
    for (mx : T?) in xs {
        if let x = mx {
            return x
        }
    }
    return nil
}
let xs : [Int?] = [nil, 4, nil]
println(some(xs))