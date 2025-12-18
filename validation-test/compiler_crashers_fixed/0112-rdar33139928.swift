// RUN: %target-swift-frontend -swift-version 4 %s -emit-ir

struct X<Elements: Sequence> { }
extension X where Elements == [Int] { }
