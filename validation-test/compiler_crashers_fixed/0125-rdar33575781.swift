// RUN: not %target-swift-frontend -typecheck %s 

public class MyCollection : MutableCollection {
  public var startIndex: MyCollection.Index
  public var endIndex: MyCollection.Index
}

