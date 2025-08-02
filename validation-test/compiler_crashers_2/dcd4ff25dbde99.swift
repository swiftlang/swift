// {"kind":"typecheck","signature":"swift::ValueDecl::isObjC() const"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
protocol a
  @objc protocol b : c
    @objc protocol d : b
      @objc protocol c : d, a
