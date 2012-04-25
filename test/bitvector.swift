// RUN: %swift -I %S %s -verify
import swift

struct BitVector64 {
  bits : Int64

  subscript (bit : Int) -> Bool {
    get {      
      if (bits & (1 << bit)) != 0 {
        return true
      }
      return false;
    }

    set {
      var mask = 1 << bit
      if value {
        bits = bits | mask
      } else {
        bits = bits & ~mask
      }  
    }
  }
}
