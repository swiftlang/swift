
import Swift

func main() {
  let f = foo() // expected-warning {{initialization of immutable value 'f' was never used}}
                // expected-note@-1{{consider replacing with '_'}}
}

main()
