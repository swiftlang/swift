// {"kind":"typecheck","signature":"swift::ide::printTypeUSR(swift::Type, llvm::raw_ostream&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@propertyWrapper struct a < b { wrappedValue : c var projectedValue init(projectedValue d) func e(@a & f g : b
