// RUN: %target-swift-emit-silgen -enable-sil-ownership -verify %s

protocol P {}

func for_loop_tuple_destructure_reabstraction(_ x: [(P, P.Protocol)]) {
  for (a, b) in x { _ = (a, b) }
}

