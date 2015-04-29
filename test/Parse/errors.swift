// RUN: %target-parse-verify-swift

enum MSV : _ErrorType {
  case Foo, Bar, Baz

  var domain: String { return "" }
  var code: Int { return 0 }
}

func opaque_error() -> _ErrorType { return MSV.Foo }

func one() {
  do {
    true ? () : throw opaque_error() // expected-error {{expected expression after '? ... :' in ternary expression}}
  } catch _ {
  }

  do {
    
  } catch { // expected-warning {{'catch' block is unreachable because no errors are thrown in 'do' block}}
    let error2 = error
  }

  do {
  } catch where true { // expected-warning {{'catch' block is unreachable because no errors are thrown in 'do' block}}
    let error2 = error
  } catch {
  }
}
