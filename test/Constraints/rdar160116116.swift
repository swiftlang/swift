// rdar://160116116
// RUN: %target-typecheck-verify-swift

func bork() {
  let array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let n = UInt32(3)

  for ndx in n..<array.count { // expected-error {{cannot convert value of type 'UInt32' to expected argument type 'Int'}}
    print(ndx)
  }
}
