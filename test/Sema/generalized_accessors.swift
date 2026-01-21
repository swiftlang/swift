// RUN: %target-typecheck-verify-swift

struct Read {
  var simpleReadImmutable: Int {
    _read {}
  }

  var redundantRead: Int {
    _read {} // expected-note {{previous definition of '_read' accessor here}}
    _read {} // expected-error {{variable already has a '_read' accessor}}
  }

  var readAndGet: Int {
    _read {} // expected-error {{variable cannot provide both a '_read' accessor and a getter}}
    get {} // expected-note {{getter defined here}}
  }

  var readAndAddress: Int {
    _read {} // expected-note {{'_read' accessor defined here}}
    unsafeAddress {} // expected-error {{variable cannot provide both an addressor and a '_read' accessor}}
  }
}

struct ReadModifiable {
  var readAndWillSet: Int {
    _read {}
    willSet {} // expected-error {{'willSet' cannot be provided together with a '_read' accessor}}
  }

  var readAndDidSet: Int {
    _read {}
    didSet {} // expected-error {{'didSet' cannot be provided together with a '_read' accessor}}
  }

  var readAndSet: Int {
    _read {}
    set {}
  }

  var readAndMutableAddress: Int {
    _read {}
    unsafeMutableAddress {}
  }

  var readAndModify: Int {
    _read {}
    _modify {}
  }
}

struct Modify {
  var modifyAlone: Int {
    _modify {} // expected-error {{variable with a '_modify' accessor must also have a getter, addressor, or 'yielding borrow' accessor}}
  }

  var getAndModify: Int {
    get {}
    _modify {}
  }

  var addressAndModify: Int {
    unsafeAddress {}
    _modify {}
  }

  var readAndModify: Int {
    _read {}
    _modify {}
  }

  var getAndRedundantModify: Int {
    get {}
    _modify {} // expected-note {{previous definition of '_modify' accessor here}}
    _modify {} // expected-error {{variable already has a '_modify' accessor}}
  }

  var getAndModifyAndMutableAddress: Int {
    get {}
    _modify {} // expected-note {{'_modify' accessor defined here}}
    unsafeMutableAddress {} // expected-error {{variable cannot provide both a mutable addressor and a '_modify' accessor}}
  }

  var getAndModifyAndSet: Int {
    get {}
    _modify {}
    set {}
  }

  var getAndNonMutatingModifyAndNonMutatingSet: Int {
    get {}
    nonmutating _modify {}
    nonmutating set {}
  }

  var getAndNonMutatingModifyAndSet: Int {
    get {}
    nonmutating _modify {} // expected-error {{'_modify' accessor cannot be 'nonmutating' when the setter is not 'nonmutating'}}
    set {} // expected-note {{setter defined here}}
  }

  var getAndModifyAndNonMutatingSet: Int {
    get {} // expected-note{{getter defined here}}
    _modify {}// expected-error {{'_modify' accessor cannot be 'mutating' when both the setter is 'nonmutating' and the getter is not 'mutating'}}
    nonmutating set {} // expected-note {{setter defined here}}
  }
}

struct ImplicitlyUnwrapped {
  var x: Int!
  var y: Int? {
    _read { yield x }
    _modify { yield &x }
  }
}
