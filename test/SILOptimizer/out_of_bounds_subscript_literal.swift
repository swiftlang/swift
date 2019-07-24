// RUN: %target-swift-frontend %s -verify -emit-sil -o

func test_1() -> Int {
  let a = [1, 2, 3]
  return a[3] // expected-error {{out of bounds access to array 'a' of size '3' at index '3'}}
}

func test_2() {
  let b = [1, 2, 3, 4][4] // expected-error {{out of bounds access to array of size '4' at index '4'}}
}

func test_3() {
  let c = [[1, 2, 3, 4, 5][5]] // expected-error {{out of bounds access to array of size '5' at index '5'}}
}
