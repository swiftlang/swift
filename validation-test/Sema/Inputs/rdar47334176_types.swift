public protocol P : class {
  associatedtype V
}

public protocol R {
  associatedtype V
}

public enum E<V> : R {}
public class C<V> : R {}
