// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s

protocol a {
  associatedtype b
}
protocol c {
  associatedtype d : a
}

struct e<f : c> where f == f.g.b {
  #^A^#
