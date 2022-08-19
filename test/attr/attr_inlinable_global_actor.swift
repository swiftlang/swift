// RUN: %target-typecheck-verify-swift -swift-version 5

// REQUIRES: concurrency

@globalActor
private struct PrivateGA { // expected-note 2 {{type declared here}}
  actor Actor {}
  static let shared = Actor()
}

@globalActor
internal struct InternalGA { // expected-note {{type declared here}}
  actor Actor {}
  static let shared = Actor()
}

@globalActor @usableFromInline
internal struct UFIGA {
  @usableFromInline actor Actor {}
  @usableFromInline static let shared = Actor()
}

@globalActor
public struct PublicGA {
  public actor Actor {}
  public static let shared = Actor()
}

// expected-error@+1 {{internal struct 'UFIStructPrivateGA' cannot have private global actor 'PrivateGA'}}
@PrivateGA @usableFromInline internal struct UFIStructPrivateGA {} // expected-error {{global actor for struct 'UFIStructPrivateGA' must be '@usableFromInline' or public}}
@InternalGA @usableFromInline internal struct UFIStructInternalGA {} // expected-error {{global actor for struct 'UFIStructInternalGA' must be '@usableFromInline' or public}}
@UFIGA @usableFromInline internal struct UFIStructUFIGA {}
@PublicGA @usableFromInline internal struct UFIStructPublicGA {}
