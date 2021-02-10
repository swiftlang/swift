// RUN: %target-swift-frontend -typecheck -verify -primary-file %s %S/Inputs/inherited-init-multifile-other.swift
// RUN: %target-swift-frontend -typecheck -verify %s -primary-file %S/Inputs/inherited-init-multifile-other.swift

func make<Result: B>(ofClass cls: Result.Type) -> Result {
    return cls.init(1)
}

func make(ofClass cls: (C & P).Type) -> C {
    return cls.init(1)
}
