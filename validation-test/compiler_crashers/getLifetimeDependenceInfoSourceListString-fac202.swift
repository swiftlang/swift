// {"kind":"typecheck","original":"729ec36f","signature":"getLifetimeDependenceInfoSourceListString(swift::LifetimeDependenceInfo const&, llvm::ArrayRef<swift::AnyFunctionType::Param>)::$_0::operator()(swift::IndexSubset*, swift::LifetimeDependenceKind) const","signatureAssert":"Assertion failed: (Index < Length && \"Invalid index!\"), function operator[]","signatureNext":"ASTPrinter::printSwiftLifetimeDependence"}
// RUN: not --crash %target-swift-frontend -typecheck %s
MutableSpan.extracting(
