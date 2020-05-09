// RUN: %target-swift-frontend -emit-syntax %s > %t.emit
// RUN: %sourcekitd-test -req=syntax-tree %s > %t.sourcekit
// RUN: diff %t.emit %t.sourcekit

#if swift(<4)
  print(1)
#endif
