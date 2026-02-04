public class Klass {}

public protocol P {
  var k : Klass { borrow mutate }
}

public struct Wrapper : P {
   var _k : Klass
   public var k : Klass {
     borrow {
       return _k
     }
     mutate {
       return &_k
     }
   }
}
