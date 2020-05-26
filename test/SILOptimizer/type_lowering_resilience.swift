// RUN: %target-swift-frontend -emit-sil -O %S/Inputs/type_lowering_resilience_other.swift -primary-file %s -enable-library-evolution

@inline(__always) public func generic<T>(_ t: T) {
  _ = Holder<T>(t)
}

public func concrete() {
  generic(Inner())
}
