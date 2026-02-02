public protocol ImportedSendableProto: Sendable { }

@MainActor public struct ImportedStruct: ImportedSendableProto { }

public struct ImportedOtherStruct { }

public protocol ImportedP { }
