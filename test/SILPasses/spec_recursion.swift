// RUN: %swift -O3 -emit-sil %s 
// Make sure we are not looping forever.
extension Array {
  @mutating
  func new_method(pred: (T, T) -> Bool, left : Int, right : Int) {
      new_method(pred, left, right);
  }

}

var x = [1]
x.new_method(<, 0, 1)

