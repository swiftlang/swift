// Only generate main entry point if we're not being built as part of the
// benchmark harness. In this case you get a binary that runs the same
// workload, except it also prints results to standard output.

#if !canImport(TestsUtils)

@main struct Main {
  static func main() async {
    await run(output: true)
  }
}

#endif