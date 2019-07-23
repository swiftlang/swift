// RUN: %target-run-simple-swift

// REQUIRES: executable_test

func test(reportError: ((String) -> (Void))? = nil) {
  let reportError = reportError ?? { error in
    print(error)
  }

  for _ in 0..<1 {
    reportError("foo bar baz")
  }
}

func main() {
  test { error in
      print(error)
  }
}

main()
