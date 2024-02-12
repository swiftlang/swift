// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token CONFORMANCE_EXT | %FileCheck %s --check-prefix=CONFORMANCE_EXT

protocol P {
  init(requirement: Int)
  init(customizable: Int)

  var requirementVar: Int { get }
  var customizableVar: Int { get }

  func requirementMethod()
  func customizableMethod()
}

extension P {
  init(customizable v: Int) { self.init(requirement: v) }
  init(nonRequirement v: Int) {  self.init(requirement: v) }

  var customizableVar: Int { 1 }
  var nonRequirementVar: Int { 1 }

  func customizableMethod() {}
  func nonRequirement() {}
}

struct S: P {
  #^CONFORMANCE_EXT^#

// CONFORMANCE_EXT-NOT: nonRequirement
// CONFORMANCE_EXT-DAG: Decl[Constructor]/Super:            init(requirement: Int) {|};
// CONFORMANCE_EXT-DAG: Decl[Constructor]/Super:            init(customizable: Int) {|};
// CONFORMANCE_EXT-DAG: Decl[InstanceVar]/Super:            var requirementVar: Int;
// CONFORMANCE_EXT-DAG: Decl[InstanceVar]/Super:            var customizableVar: Int;
// CONFORMANCE_EXT-DAG: Decl[InstanceMethod]/Super:         func requirementMethod() {|};
// CONFORMANCE_EXT-DAG: Decl[InstanceMethod]/Super:         func customizableMethod() {|};
// CONFORMANCE_EXT-NOT: nonRequirement
}
