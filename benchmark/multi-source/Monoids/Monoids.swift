/// This is the main entry point.
///
/// We enumerate all 2-generator, 2-relation monoids of length up to 10:
///
///   <a, b | u=v, w=x>  where |u| + |v| + |w| + |x| <= 10
///
/// We attempt to build an FCRS for each one by trying various strategies,
/// which ultimately succeeds for all but three instances.
func run(output: Bool) async {
  let shapes = presentationShapes(rules: 2, upToLength: 10)

  let alphabet = 2
  let instances = enumerateAll(alphabet: alphabet, shapes: shapes, output: output)

  var solver = Solver(alphabet: alphabet, instances: instances, output: output)
  await solver.solve()

  // These we know we can't solve.
  let expect: [Presentation] = ["bab=aaa,bbbb=1", "aaaa=1,abbba=b", "aaa=a,abba=bb"]

  // Make sure everything else was solved.
  let unsolved = solver.subset.map { instances[$0] }
  if unsolved != expect {
    fatalError("Expected \(expect), but got \(unsolved)")
  }
}
