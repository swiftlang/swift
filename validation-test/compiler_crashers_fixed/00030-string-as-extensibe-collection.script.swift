// RUN: %target-swift-frontend %s -emit-ir

// Test case submitted to project by https://github.com/tmu (Teemu Kurppa)

extension String : RangeReplaceableCollectionType {}

func f<S : RangeReplaceableCollectionType>(seq: S) -> S {
    return S() + seq
}

f("a")
