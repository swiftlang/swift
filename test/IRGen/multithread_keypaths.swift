// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -c %S/Inputs/multithread_keypaths_other.swift %s -num-threads 2 -o %t/1.o -o %t/2.o -module-name multithread_keypaths
// RUN: %target-swift-frontend -c %S/Inputs/multithread_keypaths_other.swift %s -num-threads 2 -o %t/1.o -o %t/2.o -module-name multithread_keypaths -enable-testing
// RUN: %target-swift-frontend -c %S/Inputs/multithread_keypaths_other.swift %s -num-threads 2 -o %t/1.o -o %t/2.o -module-name multithread_keypaths -enable-resilience
// RUN: %target-swift-frontend -c %S/Inputs/multithread_keypaths_other.swift %s -num-threads 2 -o %t/1.o -o %t/2.o -module-name multithread_keypaths -enable-testing -enable-resilience

func f(_ k: WritableKeyPath<A, Int>) {}

func g() {
  f(\A.foo)
}
