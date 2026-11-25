// {"kind":"emit-ir","signature":"swift::tbdgen::TBDGenVisitor::addSymbol(llvm::StringRef, swift::SymbolSource, llvm::MachO::SymbolFlags, llvm::MachO::EncodeKind)","signatureAssert":"Assertion failed: (false && \"TBDGen symbol appears twice\"), function addSymbolInternal","signatureNext":"TBDGenVisitor::addFunction"}
// RUN: not --crash %target-swift-frontend -emit-ir %s
@_silgen_name("c")
public func a(b: Int32) -> Int32 {
  1
}
@_silgen_name("c")
public func c(_: Int32) -> Int32 {
}
func d() -> Any {
  var e: Any = []
  return e
}
c(0)
