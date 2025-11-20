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
  let expectUnsolved: [Presentation] = ["bab=aaa,bbbb=1", "aaaa=1,abbba=b", "aaa=a,abba=bb"]

  // Make sure everything else was solved.
  let unsolved = solver.subset.map { instances[$0] }
  if unsolved != expectUnsolved {
    fatalError("Expected \(expectUnsolved), but got \(unsolved)")
  }

  // Also check finite monoid results.
  let expectFinite = [1: 1188, 2: 2059, 3: 1233, 4: 1644, 5: 1019, 6: 1630, 7: 686, 8: 884, 9: 493, 10: 615, 11: 191, 12: 317, 13: 79, 14: 134, 15: 62, 16: 158, 17: 16, 18: 60, 19: 2, 20: 52, 21: 38, 22: 12, 24: 31, 25: 5, 26: 11, 27: 42, 28: 10, 30: 52, 32: 5, 34: 8, 36: 17, 39: 4, 42: 4, 44: 8, 48: 12, 50: 12, 52: 4, 55: 4, 56: 8, 60: 18, 64: 14, 81: 6, 84: 22, 96: 1, 100: 4, 105: 2, 120: 7, 129: 2, 147: 6, 160: 2, 165: 2, 192: 2, 195: 2, 320: 2, 324: 2, 339: 4, 605: 2, 1083: 2]
  let finite = solver.finite
  if finite != expectFinite {
    fatalError("Expected \(expectFinite), but got \(finite)")
  }
}
