// rdar://191350980
// RUN: %target-typecheck-verify-swift

struct Request {}

class TransactionMetrics {
  var request: Request { Request() }
}

class TaskMetrics {
  var transactionMetrics: [TransactionMetrics] { [] }
}

func receive(metrics: TaskMetrics) {
  _ = metrics.transactionMetrics.lastObject?.request
  // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
}
