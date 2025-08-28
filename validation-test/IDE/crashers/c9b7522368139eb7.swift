// {"kind":"complete","original":"ad5b343d","signature":"swift::InterfaceTypeRequest::cacheResult(swift::Type) const","signatureAssert":"Assertion failed: (!type->is<InOutType>() && \"Interface type must be materializable\"), function cacheResult"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
[
  #^^#
].reduce?(
  [:][0] {
    $a, b in
  })
