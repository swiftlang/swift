// RUN: %target-swift-frontend -emit-silgen -verify %s

protocol P {}

func for_loop_tuple_destructure_reabstraction(_ x: [(P, P.Protocol)]) {
  for (a, b) in x { _ = (a, b) }
}

