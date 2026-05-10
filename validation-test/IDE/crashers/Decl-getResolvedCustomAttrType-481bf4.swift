// {"kind":"complete","original":"bc9977bf","signature":"swift::Decl::getResolvedCustomAttrType(swift::CustomAttr*) const","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@attached(member ) macro a = ( @a struct d { @b c { #^^#
