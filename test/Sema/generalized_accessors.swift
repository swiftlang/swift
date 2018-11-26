// RUN: %target-typecheck-verify-swift

struct Read {
  var simpleReadImmutable: Int {
    _read {}
  }

  var redundantRead: Int {
    _read {} // expected-note {{previous definition of 'read' accessor here}}
    _read {} // expected-error {{variable already has a 'read' accessor}}
  }

  var readAndGet: Int {
    _read {} // expected-error {{variable cannot provide both a 'read' accessor and a getter}}
    get {} // expected-note {{getter defined here}}
  }

  var readAndAddress: Int {
    _read {} // expected-note {{'read' accessor defined here}}
    unsafeAddress {} // expected-error {{variable cannot provide both an addressor and a 'read' accessor}}
  }
}

struct ReadModifiable {
  var readAndWillSet: Int {
    _read {}
    willSet {} // expected-error {{'willSet' cannot be provided together with a 'read' accessor}}
  }

  var readAndDidSet: Int {
    _read {}
    didSet {} // expected-error {{'didSet' cannot be provided together with a 'read' accessor}}
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
    _modify {} // expected-error {{variable with a 'modify' accessor must also have a getter, addressor, or 'read' accessor}}
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
    _modify {} // expected-note {{previous definition of 'modify' accessor here}}
    _modify {} // expected-error {{variable already has a 'modify' accessor}}
  }

  var getAndModifyAndMutableAddress: Int {
    get {}
    _modify {} // expected-note {{'modify' accessor defined here}}
    unsafeMutableAddress {} // expected-error {{variable cannot provide both a mutable addressor and a 'modify' accessor}}
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
    nonmutating _modify {} // expected-error {{'modify' accessor cannot be nonmutating when the setter is mutating}}
    set {} // expected-note {{setter defined here}}
  }

  var getAndModifyAndNonMutatingSet: Int {
    get {}
    _modify {}// expected-error {{'modify' accessor cannot be mutating when the setter is nonmutating}}
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
