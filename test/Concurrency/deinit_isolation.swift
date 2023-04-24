// RUN: %target-swift-frontend -parse-as-library -emit-sil -verify %s

// Fixtures

@globalActor final actor FirstActor {
  static let shared: FirstActor = FirstActor()
}

@globalActor final actor SecondActor {
  static let shared: SecondActor = SecondActor()
}


@FirstActor
func isolatedFunc() {}  // expected-note 10{{calls to global function 'isolatedFunc()' from outside of its actor context are implicitly asynchronous}}

class BaseWithNonisolatedDeinit {}

class BaseWithDeinitIsolatedOnFirstActor {
    @FirstActor deinit {}
}

class BaseWithDeinitIsolatedOnSecondActor {
    @SecondActor deinit {}
}

//
// Part 1 - Actors
//

actor ImplicitDeinitActor {
    // nonisolated deinit
}

actor ExplicitDeinitActor {
    // self-isolated deinit
    deinit {
        isolatedFunc() // expected-error {{call to global actor 'FirstActor'-isolated global function 'isolatedFunc()' in a synchronous actor-isolated context}}
    }
}

actor NonisolatedDeinitActor {
    // nonisolated deinit
    nonisolated deinit {
        isolatedFunc() // expected-error {{call to global actor 'FirstActor'-isolated global function 'isolatedFunc()' in a synchronous nonisolated context}}
    }
}

actor IsolatedDeinitActor {
    // FirstActor-isolated deinit
    @FirstActor deinit {
        isolatedFunc() // ok
    }
}

// Part 2 - Classes
// Part 2.1 - Without base class

@FirstActor
class ImplicitDeinit {
    // nonisolated deinit
}

@FirstActor
class ExplicitDeinit {
    // FirstActor-isolated deinit
    deinit {
        isolatedFunc() // ok
    }
}

@FirstActor
class NonisolatedDeinit {
    // nonisolated deinit
    nonisolated deinit {
        isolatedFunc() // expected-error {{call to global actor 'FirstActor'-isolated global function 'isolatedFunc()' in a synchronous nonisolated context}}
    }
}

class IsolatedDeinit {
    // FirstActor-isolated deinit
    @FirstActor deinit {
        isolatedFunc() // ok
    }
}

@FirstActor
class DifferentIsolatedDeinit {
    // SecondActor-isolated deinit
    @SecondActor deinit {
        isolatedFunc() // expected-error {{call to global actor 'FirstActor'-isolated global function 'isolatedFunc()' in a synchronous global actor 'SecondActor'-isolated context}}
    }
}

// Part 2.2 - Base class with nonisolated deinit

@FirstActor
class ImplicitDeinitInheritNonisolated: BaseWithNonisolatedDeinit {
    // nonisolated deinit
}

@FirstActor
class ExplicitDeinitInheritNonisolated: BaseWithNonisolatedDeinit {
    // FirstActor-isolated deinit
    deinit {
        isolatedFunc() // ok
    }
}

@FirstActor
class NonisolatedDeinitInheritNonisolated: BaseWithNonisolatedDeinit {
    // nonisolated deinit
    nonisolated deinit {
        isolatedFunc() // expected-error {{call to global actor 'FirstActor'-isolated global function 'isolatedFunc()' in a synchronous nonisolated context}}
    }
}

class IsolatedDeinitInheritNonisolated: BaseWithNonisolatedDeinit {
    // FirstActor-isolated deinit
    @FirstActor deinit {
        isolatedFunc() // ok
    }
}

@FirstActor
class DifferentIsolatedDeinitInheritNonisolated: BaseWithNonisolatedDeinit {
    // SecondActor-isolated deinit
    @SecondActor deinit {
        isolatedFunc() // expected-error {{call to global actor 'FirstActor'-isolated global function 'isolatedFunc()' in a synchronous global actor 'SecondActor'-isolated context}}
    }
}

// Part 2.3 - Base class with isolated deinit

@FirstActor
class ImplicitDeinitInheritIsolated1: BaseWithDeinitIsolatedOnFirstActor {
    // FirstActor-isolated deinit
}

@FirstActor
class ExplicitDeinitIsolated1: BaseWithDeinitIsolatedOnFirstActor {
    // FirstActor-isolated deinit
    deinit {
        isolatedFunc() // ok
    }
}

@FirstActor
class NonisolatedDeinitIsolated1: BaseWithDeinitIsolatedOnFirstActor {
    // FirstActor-isolated deinit
    nonisolated deinit { // expected-error {{nonisolated deinitializer 'deinit' has different actor isolation from global actor 'FirstActor'-isolated overridden declaration}}
        isolatedFunc() // expected-error {{call to global actor 'FirstActor'-isolated global function 'isolatedFunc()' in a synchronous nonisolated context}}
    }
}

class IsolatedDeinitIsolated1: BaseWithDeinitIsolatedOnFirstActor {
    // FirstActor-isolated deinit
    @FirstActor deinit {
        isolatedFunc() // pl
    }
}

@FirstActor
class DifferentIsolatedDeinitIsolated1: BaseWithDeinitIsolatedOnFirstActor {
    // error
    @SecondActor deinit { // expected-error {{global actor 'SecondActor'-isolated deinitializer 'deinit' has different actor isolation from global actor 'FirstActor'-isolated overridden declaration}}
        isolatedFunc() // expected-error {{call to global actor 'FirstActor'-isolated global function 'isolatedFunc()' in a synchronous global actor 'SecondActor'-isolated context}}
    }
}

// Part 2.4 - Base class with isolated deinit with different actor

@FirstActor
class ImplicitDeinitInheritIsolated2: BaseWithDeinitIsolatedOnSecondActor {
    // SecondActor-isolated deinit
}

@FirstActor
class ExplicitDeinitIsolated2: BaseWithDeinitIsolatedOnSecondActor {
    // SecondActor-isolated deinit
    deinit {
        isolatedFunc() // ok
    }
}

@FirstActor
class NonisolatedDeinitIsolated2: BaseWithDeinitIsolatedOnSecondActor {
    // SecondActor-isolated deinit
    nonisolated deinit {
        isolatedFunc() // expected-error {{call to global actor 'FirstActor'-isolated global function 'isolatedFunc()' in a synchronous nonisolated context}}
    }
}

class IsolatedDeinitIsolated2: BaseWithDeinitIsolatedOnSecondActor {
    // // error
    @FirstActor deinit {
        isolatedFunc() // error
    }
}

@FirstActor
class DifferentIsolatedDeinitIsolated2: BaseWithDeinitIsolatedOnSecondActor {
    // SecondActor-isolated deinit
    @SecondActor deinit {
        isolatedFunc() // expected-error {{call to global actor 'FirstActor'-isolated global function 'isolatedFunc()' in a synchronous global actor 'SecondActor'-isolated context}}
    }
}

