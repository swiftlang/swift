// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename=%s

protocol ProviderP {
  associatedtype Data
}

protocol BaseP {}
protocol DerivedP : BaseP  {
  associatedtype Provider: ProviderP

  func testing(_: Provider.Data)
}

struct Concrete : DerivedP {
  #^COMPLETE^#
}
