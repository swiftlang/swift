public class C {
  public func f1() {
     { (error : NSError?) -> Void in
      dispatch_async(dispatch_get_main_queue(), { () -> Void in
      })
    }
  }
}
