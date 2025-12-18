// RUN: not %target-swift-frontend -typecheck %s

class DataType: DataType {}
extension DataType: Encodable {}
