
@MainActor
public func mainActorDefaultArg() -> Int { 0 }

@MainActor
public func useMainActorDefault(_ value: Int = mainActorDefaultArg()) {}

public func nonisolatedDefaultArg() -> Int { 0 }

@MainActor
public func useNonisolatedDefault(_ value: Int = nonisolatedDefaultArg()) {}
