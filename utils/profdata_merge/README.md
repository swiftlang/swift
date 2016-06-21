# Profdata Merge

Because LLVM's instrumented builds produce a `profraw` file every time a
they are executed, the Swift test suite, when run with coverage enabled,
produces thousands of 30MB profile files.

When build-script is run with `--swift-analyze-code-coverage merged`, this
module will get called before the tests run and spin up a small server that
runs alongside the test suite. The server accepts filenames that it receives
after each test directory is finished executing, and feeds them to 10 worker
processes which concurrently merge those files together. Doing so means the
test results directory doesn't balloon in size, as the files are merged together
and removed often.

Once the test suite is finished, it sends a crafted message to the server that
tells it to merge any files left in the queue, then merges all the workers
together into one final file.
