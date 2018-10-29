// RUN: %empty-directory(%t)
// RUN: %validate-incrparse %s --test-case MULTI

let one: Int;<reparse MULTI>let two: Int; let three: Int; <<MULTI<|||                      >>><<MULTI<|||   >>>let found: Int;</reparse MULTI>let five: Int;
