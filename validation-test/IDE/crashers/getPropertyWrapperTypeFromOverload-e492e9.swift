// {"kind":"complete","original":"f28300c6","signature":"getPropertyWrapperTypeFromOverload(swift::constraints::ConstraintSystem&, swift::constraints::SelectedOverload, llvm::function_ref<swift::VarDecl* (swift::VarDecl*)>)","signatureAssert":"Assertion failed: (!cs.hasType(D) && \"Should have recorded type for wrapper var\"), function getPropertyWrapperTypeFromOverload","signatureNext":"ConstraintSystem::getPropertyWrapperBackingType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@propertyWrapper struct a<b> {
  var wrappedValue: b
  class c {
    #^^#@a    var d: Double = Double(d)
  }
}
