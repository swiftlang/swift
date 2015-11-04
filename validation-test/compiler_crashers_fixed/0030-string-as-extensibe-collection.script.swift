// RUN: %target-swift-frontend %s -emit-ir

// Test case submitted to project by https://github.com/tmu (Teemu Kurppa)

extension String : RangeReplaceableCollection {}

func f<S : RangeReplaceableCollection>(seq: S) -> S {
    return S() + seq
}

f("a")
