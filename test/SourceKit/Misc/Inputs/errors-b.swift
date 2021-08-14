public func invalidFuncBody() { undefined }

public func invalidFuncSignature(undefined '' undefined) {}

public func invalidFuncThrows() throws -> undefined { throw undefined }

public func invalidFuncType() -> undefined {}

public func invalidGenericFuncBody<T>(param: T) -> T { undefined }

public func invalidGenericFuncType<T>(param: T) -> undefined {}

public let invalidGlobalClosureBody = { arg -> Int in
  undefined
}

public let invalidGlobalClosureType = { -> in }

public let invalidGlobalKeypath = InvalidStruct\.undefined

public let invalidGlobalMissingInit: String =

public func invalidPartialFunc

public func typeUsesFunc(pe: InvalidEnum, pa: InvalidAlias,
                         pp: InvalidProtocol, ps: InvalidStruct,
                         pg: GenericInvalidStruct<InvalidStruct, InvalidStruct>,
                         pc: InvalidClass) -> Int {}
