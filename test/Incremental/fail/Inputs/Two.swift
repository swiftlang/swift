final public class Subclass: Base {}

public protocol PublicProtocol: BaseProtocol {}

// expected-no-dependency {{main.BaseProtocol}}
