
import Swift // expected-remark {{rebuilding module 'Swift'}}
// expected-note @-1 {{compiled module is out of date}}
// expected-note @-2 {{unable to load compiled module}}
// expected-note @-3 {{prebuilt module is out of date}}
// expected-note @-4 {{unable to load compiled module}}

func main() {
  let f = foo() // expected-warning {{initialization of immutable value 'f' was never used}}
}

main()
