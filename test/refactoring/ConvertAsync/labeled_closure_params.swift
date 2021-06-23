// RUN: %empty-directory(%t)

// RUN: %refactor-check-compiles -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=MULTIPLE-LABELED-RESULTS %s
func mutlipleLabeledResults(completion: (_ first: String, _ second: String) -> Void) { }
// MULTIPLE-LABELED-RESULTS: {
// MULTIPLE-LABELED-RESULTS-NEXT: async {
// MULTIPLE-LABELED-RESULTS-NEXT: let result = await mutlipleLabeledResults()
// MULTIPLE-LABELED-RESULTS-NEXT: completion(result.first, result.second)
// MULTIPLE-LABELED-RESULTS-NEXT: }
// MULTIPLE-LABELED-RESULTS-NEXT: }
// MULTIPLE-LABELED-RESULTS: func mutlipleLabeledResults() async -> (first: String, second: String) { }

// RUN: %refactor-check-compiles -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=MIXED-LABELED-RESULTS %s
func mixedLabeledResult(completion: (_ first: String, String) -> Void) { }
// MIXED-LABELED-RESULTS: {
// MIXED-LABELED-RESULTS-NEXT: async {
// MIXED-LABELED-RESULTS-NEXT: let result = await mixedLabeledResult()
// MIXED-LABELED-RESULTS-NEXT: completion(result.first, result.1)
// MIXED-LABELED-RESULTS-NEXT: }
// MIXED-LABELED-RESULTS-NEXT: }
// MIXED-LABELED-RESULTS: func mixedLabeledResult() async -> (first: String, String) { }

// RUN: %refactor-check-compiles -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=SINGLE-LABELED-RESULT %s
func singleLabeledResult(completion: (_ first: String) -> Void) { }
// SINGLE-LABELED-RESULT: {
// SINGLE-LABELED-RESULT-NEXT: async {
// SINGLE-LABELED-RESULT-NEXT: let result = await singleLabeledResult()
// SINGLE-LABELED-RESULT-NEXT: completion(result)
// SINGLE-LABELED-RESULT-NEXT: }
// SINGLE-LABELED-RESULT-NEXT: }
// SINGLE-LABELED-RESULT: func singleLabeledResult() async -> String { }

// RUN: %refactor-check-compiles -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=SINGLE-LABELED-RESULT-WITH-ERROR %s
func singleLabeledResultWithError(completion: (_ first: String?, _ error: Error?) -> Void) { }
// SINGLE-LABELED-RESULT-WITH-ERROR: {
// SINGLE-LABELED-RESULT-WITH-ERROR-NEXT: async {
// SINGLE-LABELED-RESULT-WITH-ERROR-NEXT: do {
// SINGLE-LABELED-RESULT-WITH-ERROR-NEXT: let result = try await singleLabeledResultWithError()
// SINGLE-LABELED-RESULT-WITH-ERROR-NEXT: completion(result, nil)
// SINGLE-LABELED-RESULT-WITH-ERROR-NEXT: } catch {
// SINGLE-LABELED-RESULT-WITH-ERROR-NEXT: completion(nil, error)
// SINGLE-LABELED-RESULT-WITH-ERROR-NEXT: }
// SINGLE-LABELED-RESULT-WITH-ERROR-NEXT: }
// SINGLE-LABELED-RESULT-WITH-ERROR-NEXT: }
// SINGLE-LABELED-RESULT-WITH-ERROR: func singleLabeledResultWithError() async throws -> String { }

// RUN: %refactor-check-compiles -add-async-alternative -dump-text -source-filename %s -pos=%(line+1):1 | %FileCheck -check-prefix=MULTIPLE-LABELED-RESULT-WITH-ERROR %s
func multipleLabeledResultWithError(completion: (_ first: String?, _ second: String?, _ error: Error?) -> Void) { }
// MULTIPLE-LABELED-RESULT-WITH-ERROR: {
// MULTIPLE-LABELED-RESULT-WITH-ERROR-NEXT: async {
// MULTIPLE-LABELED-RESULT-WITH-ERROR-NEXT: do {
// MULTIPLE-LABELED-RESULT-WITH-ERROR-NEXT: let result = try await multipleLabeledResultWithError()
// MULTIPLE-LABELED-RESULT-WITH-ERROR-NEXT: completion(result.first, result.second, nil)
// MULTIPLE-LABELED-RESULT-WITH-ERROR-NEXT: } catch {
// MULTIPLE-LABELED-RESULT-WITH-ERROR-NEXT: completion(nil, nil, error)
// MULTIPLE-LABELED-RESULT-WITH-ERROR-NEXT: }
// MULTIPLE-LABELED-RESULT-WITH-ERROR-NEXT: }
// MULTIPLE-LABELED-RESULT-WITH-ERROR-NEXT: }
// MULTIPLE-LABELED-RESULT-WITH-ERROR: func multipleLabeledResultWithError() async throws -> (first: String, second: String) { }

func testConvertCall() {
  // RUN: %refactor -convert-call-to-async-alternative -dump-text -source-filename %s -pos=%(line+1):3 | %FileCheck -check-prefix=CONVERT-CALL %s
  mutlipleLabeledResults() { (a, b) in
    print(a)
    print(b)
  }
  // CONVERT-CALL: let (a, b) = await mutlipleLabeledResults()
  // CONVERT-CALL-NEXT: print(a)
  // CONVERT-CALL-NEXT: print(b)
}