// {"kind":"typecheck","original":"123fe156","signature":"swift::Mangle::ASTMangler::appendAccessorEntity(llvm::StringRef, swift::AbstractStorageDecl const*, bool)","stackOverflow":true}
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol Harvestable {
}
func a() {
  let b: Collection<some Harvestable>
}
