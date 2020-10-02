// RUN: %empty-directory(%t)
// RUN: %validate-incrparse %s --test-case REPLACE


var value: Int {
    get { fatalError() }
<<REPLACE<|||}>>>

let x = 10
