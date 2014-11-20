// RUN: %target-swift-frontend %s -emit-ir

// Test case submitted to project by https://github.com/tmu (Teemu Kurppa)

func f<S : ExtensibleCollectionType>(seq: S) -> S {
    return S() + seq
}

f("a")
