By submitting a pull request, you represent that you have the right to license
your contribution to Apple and the community, and agree by submitting the patch
that your contributions are licensed under the [Swift
license](https://swift.org/LICENSE.txt).

---

Before submitting a pull request, please make sure that your changes follow the
[Swift project guidelines for contributing code](https://swift.org/contributing/#contributing-code).

We request that code changes unrelated to Swift for TensorFlow be submitted to
the [upstream Swift repository](https://github.com/apple/swift). For example,
code formatting changes that do not affect Swift for TensorFlow source code
should be submitted upstream.

It is a good idea to discuss any non-trivial submissions with the project
maintainers before submitting a pull request: please join the
[swift@tensorflow.org](https://groups.google.com/a/tensorflow.org/d/forum/swift)
mailing list to participate in discussions.

All changes to existing Swift source code should be marked clearly with a
`SWIFT_ENABLE_TENSORFLOW` comment at the top of every diff hunk. This makes
it easier to merge from upstream.

Continuous integration (CI) is still being set up, so if you submit a pull
request with non-trivial changes that require test coverage, please anticipate
some delay before we can properly review and merge it.
