public protocol GenericSubscriptProto {
  subscript<K, V>(k: K) -> V { get set }
}
