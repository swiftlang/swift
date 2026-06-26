// {"kind":"complete","original":"25845ee9","signature":"swift::GenericParamListRequest::cacheResult(swift::GenericParamList*) const","signatureAssert":"Assertion failed: (context->GenericParamsAndState.getInt() == GenericParamsState::Parsed), function cacheResult","signatureNext":"GenericContext::getGenericParams"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
// REQUIRES: OS=macosx
@attached(extension) macro a
struct d
  @a extension d {
    b
    extension c {
      init {
        #^^#
