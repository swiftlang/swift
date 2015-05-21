import Foundation

public class ObjCSubclass : NSObject {
  // This properties will have a non constant field access due to the objc super
  // class.
  public final var field : Int32

  public override init () {
    field = 10
  }

};
