// {"signature":"swift::constraints::DisjunctionChoiceProducer::partitionGenericOperators(unsigned int*, unsigned int*)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
{
  print($0) $00 + 0. / 1
