// expected-warning@<unknown> * {{libc not found for }}

import Swift

func main() {
  let f = foo() // expected-warning {{initialization of immutable value 'f' was never used}}
}

main()
