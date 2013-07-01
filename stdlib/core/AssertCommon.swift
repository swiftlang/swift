/// True if we are going to print a message about a failed assertion.
///
/// FIXME: this should be an atomic bool.
var _in_assert_fail : Bool = false

func _assert_fail(message: String, file: String, line: Int) {
  if _in_assert_fail {
    // Prevent recursive assertions.  This can happen if the original one was
    // triggered by String or print().
    //
    // FIXME: this should be implemented with compare-and-exchange.
    abort()
  }
  _in_assert_fail = true
  if message.isEmpty() {
    print("assertion failed: file \(file), line \(line)\n")
  } else {
    print("assertion failed: \(message): file \(file), line \(line)\n")
  }
}

