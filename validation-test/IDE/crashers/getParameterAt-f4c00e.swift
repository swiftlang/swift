// {"kind":"complete","original":"08e5e9f4","signature":"swift::getParameterAt(swift::ConcreteDeclRef, unsigned int)","signatureNext":"ArgumentTypeCheckCompletionCallback::sawSolutionImpl"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
{
  func a<each b, each c>(
    repeat each b, repeat each c
  ) -> (repeat (each b each c
  let _  a(1
  ""
  ,
  #^^#
  d
  : e, 2.0
