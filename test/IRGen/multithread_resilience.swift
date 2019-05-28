// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -c %S/Inputs/multithread_resilience_other.swift %s -num-threads 2 -o %t/1.o -o %t/2.o -module-name multithread_resilience
// RUN: %target-swift-frontend -c %S/Inputs/multithread_resilience_other.swift %s -num-threads 2 -o %t/1.o -o %t/2.o -module-name multithread_resilience -enable-testing
// RUN: %target-swift-frontend -c %S/Inputs/multithread_resilience_other.swift %s -num-threads 2 -o %t/1.o -o %t/2.o -module-name multithread_resilience -enable-library-evolution
// RUN: %target-swift-frontend -c %S/Inputs/multithread_resilience_other.swift %s -num-threads 2 -o %t/1.o -o %t/2.o -module-name multithread_resilience -enable-testing -enable-library-evolution

public protocol Defaultable {
  func defaulted()
}
