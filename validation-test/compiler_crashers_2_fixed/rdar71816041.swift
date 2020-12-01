// RUN: %target-swift-frontend -emit-ir -primary-file %s -enable-experimental-concurrency
// REQUIRES: concurrency

func getIntAndString() async -> (Int, String) { (5, "1") }

func testDecompose() async -> Int {
  async let (i, s) = await getIntAndString()
  return await i
}
