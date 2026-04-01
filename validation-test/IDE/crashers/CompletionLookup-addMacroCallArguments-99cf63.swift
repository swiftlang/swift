// {"kind":"complete","original":"760fd217","signature":"swift::ide::CompletionLookup::addMacroCallArguments(swift::MacroDecl const*, swift::DeclVisibilityKind, bool)","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"CompletionLookup::foundDecl"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@attached(extension ) macro a( b = @a
enum b
  @
  #^^#
