func _unittest_fail<T : FormattedPrintable>(message: String,
                                            expected: T, actual: T,
                                            user_message: String,
                                            file: String, line: Int) {
  print(message)
  if !user_message.isEmpty() {
    println(": \(user_message)")
  }
  println("File \(file), line \(line)")

  // FIXME: this should use string interpolation.
  print("Expected: ")
  println(expected.format('v', ""))
  print("Actual: ")
  println(actual.format('v', ""))
}

func ASSERT_eq<T : protocol<Equatable, FormattedPrintable>>
    (expected: T, actual: T, message: String = "",
     file: String = __FILE__,
     line: Int = __LINE__) {
  if expected == actual {
    return
  }
  _unittest_fail("Unit test ASSERT failed: values should be equal",
                 expected, actual,
                 message, file, line)
  abort()
}

func ASSERT_ne<T : protocol<Equatable, FormattedPrintable>>
    (expected: T, actual: T, message: String = "",
     file: String = __FILE__,
     line: Int = __LINE__) {
  if expected != actual {
    return
  }
  _unittest_fail("Unit test ASSERT failed: values should not be equal",
                 expected, actual,
                 message, file, line)
  abort()
}

func ASSERT_true(actual: Bool, message: String = "",
                 file: String = __FILE__,
                 line: Int = __LINE__) {
  if actual {
    return
  }
  println("Unit test ASSERT failed: expected true")
  println("File \(file), line \(line)")
  abort()
}

func ASSERT_false(actual: Bool, message: String = "",
                  file: String = __FILE__,
                  line: Int = __LINE__) {
  if !actual {
    return
  }
  println("Unit test ASSERT failed: expected false")
  println("File \(file), line \(line)")
  abort()
}

func ASSERT_not_reached(message: String = "",
                        file: String = __FILE__,
                        line: Int = __LINE__) {
  println("Unit test ASSERT failed: statement reached")
  abort()
}

// FIXME: this should be atomic
var _any_expect_failed : Bool = false

func EXPECT_eq<T : protocol<Equatable, FormattedPrintable>>
    (expected: T, actual: T, message: String = "",
     file: String = __FILE__,
     line: Int = __LINE__) {
  if expected == actual {
    return
  }
  _unittest_fail("Unit test EXPECT failed: values should be equal",
                 expected, actual,
                 message, file, line)
  _any_expect_failed = true
}

func EXPECT_ne<T : protocol<Equatable, FormattedPrintable>>
    (expected: T, actual: T, message: String = "",
     file: String = __FILE__,
     line: Int = __LINE__) {
  if expected != actual {
    return
  }
  _unittest_fail("Unit test EXPECT failed: values should not be equal",
                 expected, actual,
                 message, file, line)
  _any_expect_failed = true
}

func EXPECT_true(actual: Bool, message: String = "",
                 file: String = __FILE__,
                 line: Int = __LINE__) {
  if actual {
    return
  }
  println("Unit test EXPECT failed: expected true")
  println("File \(file), line \(line)")
  _any_expect_failed = true
}

func EXPECT_false(actual: Bool, message: String = "",
                  file: String = __FILE__,
                  line: Int = __LINE__) {
  if !actual {
    return
  }
  println("Unit test EXPECT failed: expected false")
  println("File \(file), line \(line)")
  _any_expect_failed = true
}

func EXPECT_not_reached(message: String = "",
                        file: String = __FILE__,
                        line: Int = __LINE__) {
  println("Unit test EXPECT failed: statement reached")
  println("File \(file), line \(line)")
  _any_expect_failed = true
}

struct _Test {
  var test_case_name: String
  var test_name: String
  var f: () -> ()
}

var _tests : Vector<_Test> = Vector<_Test>()

func TEST(test_case_name: String, test_name: String, f: () -> ()) {
  _tests.append(_Test(test_case_name, test_name, f))
}

func RUN_TESTS() {
  var any_test_failed = false
  for t in _tests {
    var full_test_name = "\(t.test_case_name).\(t.test_name)"
    println("[ RUN      ] \(full_test_name)")
    _any_expect_failed = false
    t.f()
    if _any_expect_failed {
      any_test_failed = true
      println("[     FAIL ] \(full_test_name)")
    } else {
      println("[       OK ] \(full_test_name)")
    }
  }
  if any_test_failed {
    println("There were failures, aborting")
    abort()
  }
  println("All tests passed")
}

