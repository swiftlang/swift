public func libraryVersion() -> Int {
#if BEFORE
  return 0
#else
  return 10
#endif
}

#if BEFORE

public protocol Bird {
  func eat(_ food: Int) -> Int
  func containsChirps() -> Bool
}

#else

@reparentable
public protocol SeedEater {
  func eatSeed(_ food: Int) -> Int

  associatedtype Kind: AsInt

  var seedKinds: Kind { get }
  func containsChirps() -> Bool
}

extension SeedEater {
  public func containsChirps() -> Bool { return false }
}

public protocol Bird: SeedEater {
  func eat(_ food: Int) -> Int
  func containsChirps() -> Bool
}

extension Bird : @reparented SeedEater where Kind == Int {
  public func eatSeed(_ food: Int) -> Int { return eat(food + libraryVersion()) }

  public var seedKinds: Int {
    get { return libraryVersion() }
  }
}

public protocol AsInt {
  func asInt() -> Int
}

extension Int: AsInt {
  public func asInt() -> Int { return self }
}

#endif

extension Bird {
  public func containsChirps() -> Bool { return true }
}

public struct SomeType {
  public static func feed(_ b: some Bird, _ clientVersion: Int) -> Int {
  precondition(b.containsChirps())

  #if BEFORE
    return b.eat(clientVersion)
  #else
    precondition(asBase(b).containsChirps())
    let ans1 = asBase(b).eatSeed(clientVersion)
    let ans2 = feedAsBase_Param(b, clientVersion)
    precondition(ans1 == ans2)
    return ans1
  #endif
  }

  public static func count(_ b: some Bird, _ clientVersion: Int) -> Int {
  #if BEFORE
    return clientVersion
  #else
    return clientVersion + asBase(b).seedKinds.asInt()
  #endif
  }

  #if !BEFORE
  private static func asBase(_ b: some Bird) -> some SeedEater { return b }

  private static func feedAsBase_Param(_ se: some SeedEater, _ food: Int) -> Int {
    return se.eatSeed(food)
  }
  #endif
}



public struct ExistentialType {
  public static func feed(_ b: any Bird, _ clientVersion: Int) -> Int {
  precondition(b.containsChirps())

  #if BEFORE
    return b.eat(clientVersion)
  #else
    precondition(asExtBase(b).containsChirps())
    return asExtBase(b).eatSeed(clientVersion)
  #endif
  }

  public static func count(_ b: some Bird, _ clientVersion: Int) -> Int {
  #if BEFORE
    return clientVersion
  #else
    return clientVersion + asExtBase(b).seedKinds.asInt()
  #endif
  }

  #if !BEFORE
  private static func asExtBase(_ b: any Bird) -> any SeedEater { return b }
  #endif
}

#if !BEFORE
public func willChirp(_ e: some SeedEater) -> Bool {
  return e.containsChirps()
}
#endif


// Ensure you can reparent a protocol that previously inherited from
// other reparentable protocols.

@reparentable
public protocol A {
  func helloA() -> Int
}

#if !BEFORE
@reparentable
public protocol B {
  func helloB(extra: Int) -> Int
}
#endif

public protocol C {
  func helloC() -> Int
}

@reparentable
public protocol D {
  func helloD(extra: Int) -> Int
}

@reparentable
public protocol E {
  func helloE() -> Int
}

#if BEFORE
public protocol MultiParent: A, C, D, E {}
#else
public protocol MultiParent: A, B, C, D, E {}

extension MultiParent: @reparented B {
  public func helloB(extra: Int) -> Int {
    return libraryVersion() + (extra - 19)
  }
}
#endif

// Tests a pre-existing reparenting by D, ensuring we can reparent something that's been reparented.
extension MultiParent: @reparented D {
  public func helloD(extra: Int) -> Int { return 10 + (extra - 42) }
}

// The "extra:" parameters test to ensure we call the _right_ witness function, by
// passing in the correct extra value to cancel-out the subtraction in the expected implementation.

public func library_testMultiParent_someType(_ t: some MultiParent) -> Int {
  let baseline = t.helloA() + t.helloC() + t.helloD(extra: 42) + t.helloE()
  #if BEFORE
    return baseline
  #else
    return baseline + t.helloB(extra: 19)
  #endif
}

public func library_testMultiParent_anyType(_ t: any MultiParent) -> Int {
  let baseline = t.helloA() + t.helloC() + t.helloD(extra: 42) + t.helloE()
  #if BEFORE
    return baseline
  #else
    return baseline + t.helloB(extra: 19)
  #endif
}

