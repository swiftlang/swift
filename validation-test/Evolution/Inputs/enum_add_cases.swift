public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

public var starfishCount: Int = 0

public class Starfish {
  public init() {
    starfishCount += 1
  }

  deinit {
    starfishCount -= 1
  }
}

///////////////////////////////////////////////////////////////////////

public enum AddNoPayloadToSingleton {
  case Noses
#if AFTER
  case Tummies
  case Tails
#endif
}

public func addNoPayloadToSingletonCases() -> [AddNoPayloadToSingleton] {
#if BEFORE
  return [.Noses]
#else
  return [.Noses, .Tummies, .Tails]
#endif
}

///////////////////////////////////////////////////////////////////////

public enum AddPayloadToSingleton {
  case Cats
#if AFTER
  case Horses(Starfish)
#endif
}

public func addPayloadToSingletonCases(s: Starfish)
    -> [AddPayloadToSingleton] {
#if BEFORE
  return [.Cats]
#else
  return [.Cats, .Horses(s)]
#endif
}

///////////////////////////////////////////////////////////////////////

public enum AddPayloadsToSingleton {
  case Cats
#if AFTER
  case Horses(Starfish)
  case Foxes(Starfish, Starfish)
#endif
}

public func addPayloadsToSingletonCases(s: Starfish)
    -> [AddPayloadsToSingleton] {
#if BEFORE
  return [.Cats]
#else
  return [.Cats, .Horses(s), .Foxes(s, s)]
#endif
}

///////////////////////////////////////////////////////////////////////

public enum AddNoPayloadToSinglePayload {
  case Cats(Starfish)
  case Noses
#if AFTER
  case Tummies
#endif
}

public func addNoPayloadToSinglePayloadCases(s: Starfish)
    -> [AddNoPayloadToSinglePayload] {
#if BEFORE
  return [.Cats(s), .Noses]
#else
  return [.Cats(s), .Noses, .Tummies]
#endif
}

///////////////////////////////////////////////////////////////////////

public enum AddPayloadToSinglePayload {
  case Cats
  case Paws(Starfish)
#if AFTER
  case Tails(Starfish)
#endif
}

public func addPayloadToSinglePayloadCases(s: Starfish)
    -> [AddPayloadToSinglePayload] {
#if BEFORE
  return [.Cats, .Paws(s)]
#else
  return [.Cats, .Paws(s), .Tails(s)]
#endif
}

///////////////////////////////////////////////////////////////////////

public enum AddNoPayloadToMultiPayload {
  case Cats(Starfish)
  case Puppies(Starfish)
#if AFTER
  case Lambs
  case Piglets
#endif
}

public func addNoPayloadToMultiPayloadCases(s: Starfish)
    -> [AddNoPayloadToMultiPayload] {
#if BEFORE
  return [.Cats(s), .Puppies(s)]
#else
  return [.Cats(s), .Puppies(s), .Lambs, .Piglets]
#endif
}

///////////////////////////////////////////////////////////////////////

public enum AddPayloadToMultiPayload {
  case Cats(Starfish)
  case Ponies(Starfish)
  case Pandas
#if AFTER
  case Piglets(Int64)
#endif
}

public func addPayloadToMultiPayloadCases(s: Starfish)
    -> [AddPayloadToMultiPayload] {
#if BEFORE
  return [.Cats(s), .Ponies(s), .Pandas]
#else
  return [.Cats(s), .Ponies(s), .Pandas, .Piglets(1234321)]
#endif
}
