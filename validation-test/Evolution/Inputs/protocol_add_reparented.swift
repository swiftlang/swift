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
}

#else

@reparentable
public protocol SeedEater {
  func eatSeed(_ food: Int) -> Int

  associatedtype Kind: AsInt

  var seedKinds: Kind { get }
}

public protocol Bird: SeedEater {
  func eat(_ food: Int) -> Int
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

public struct SomeType {
  public static func feed(_ b: some Bird, _ clientVersion: Int) -> Int {
  #if BEFORE
    return b.eat(clientVersion)
  #else
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
  #if BEFORE
    return b.eat(clientVersion)
  #else
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


