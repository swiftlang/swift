// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-object -O %s

// This is a compile-only test. It checks that the compiler does not crash for
// a (not executed) bitcast with different sizes. This appears in the
// specialized version fo myDictionaryBridge.
// <rdar://problem/17821040>

// A minimized version of _dictionaryBridgeToObjectiveC that used to be in the
// stdlib 
public func myDictionaryBridge<
    SrcType, DestType
>(
    _ source: Dictionary<SrcType, Int>, _ keyBridgesDirectly : Bool
) -> DestType? {

  for (key, value) in source {
    if keyBridgesDirectly {
      var bridgedKey = unsafeBitCast(key, to: DestType.self)
      return bridgedKey
    }
  }
  return nil
}

var dict1 = Dictionary<String, Int>()

var res : Int? = myDictionaryBridge(dict1, false)

