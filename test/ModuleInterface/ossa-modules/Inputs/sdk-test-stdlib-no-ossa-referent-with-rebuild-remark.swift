
import Swift // expected-remark {{rebuilding module 'Swift'}}

func main() {
  let f = foo() // expected-warning {{initialization of immutable value 'f' was never used}}
}

main()
