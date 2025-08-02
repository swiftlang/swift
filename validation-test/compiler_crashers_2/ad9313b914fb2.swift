// {"kind":"typecheck","signature":"swift::Evaluator::diagnoseCycle(swift::ActiveRequest const&)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
class a
  let _ b = a
  class b : b.c
