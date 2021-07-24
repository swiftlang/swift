// REQUIRES: concurrency

func simple(completion: @escaping (String?, Error?) -> Void) { }

func mismatches() {
  // RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):3
  simple()

  // RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):3
  simple(10) { res, err in
    print("call mismatch")
  }

  // RUN: not %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):3
  simple { res in
    print("closure mismatch")
  }
}

// RUN: not %refactor -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1
func missingBody(complete: @escaping (Int?, Error?) -> Void)
