## known_issues

The `known_issues` directory is used to track known bugs that are **not**
crashers.

* If you fix a test in this directory, i.e., the test starts failing, _please_
  delete the test file and reorganise the test case by moving it to an
  appropriate place in the primary test suite under `../test`.

* If you add a test to this directory, _please_ make sure that the test is
  associated with either a
  [GitHub issue](https://github.com/swiftlang/swift/issues)
  or an Apple radar of internal origin, and include link to that record in the
  test.

  If the test is associated only with an internal report, _please_ also include
  a brief explanation of the issue in the test for clarity.
