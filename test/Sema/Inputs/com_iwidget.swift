import COM

@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol IWidget: IUnknown { }

// Reference IID from this file too so that, when this file is a SECONDARY
// input, the reference still resolves via lookup-driven synthesis.
public func f() -> GUID { return IWidget.IID }
