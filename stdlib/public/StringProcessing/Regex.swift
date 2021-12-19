import Swift
import _MatchingEngine

@frozen
public struct Regex<Match> {
  @usableFromInline
  init(_regexString: String) {}
}

public enum DynamicCaptures {}
