// RUN: %target-swift-frontend -emit-sil %s -verify

while true {}
// FIXME: Raises spurious "always true" warning. rdar://problem/19664185
while true {} // expected-note{{always evaluates to true}}
