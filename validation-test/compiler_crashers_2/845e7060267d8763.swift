// {"kind":"emit-silgen","original":"7b4a72a9","signature":"swift::getSILValueOwnership(llvm::ArrayRef<swift::SILValue>, swift::SILType)"}
// RUN: not --crash %target-swift-frontend -emit-silgen %s
var a: Any = ["": [a]] as [String: [AnyObject?]]
