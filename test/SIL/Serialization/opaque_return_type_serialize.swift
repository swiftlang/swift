// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %s -disable-availability-checking

// We substitute away opaque archetypes after serialization;
// make sure this correctly handles unlowered types like
// AST functions and packs.

public func horse<T>(_: T) {}

@_transparent public func packCallee<each T>(_ t: repeat each T) {
  repeat horse(each t)
}

@inlinable public func packCaller() {
  packCallee(opaque())
}

public func opaque() -> some Any { return 3 }
