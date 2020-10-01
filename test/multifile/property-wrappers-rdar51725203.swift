// RUN: %target-swift-frontend -typecheck -verify -primary-file %s %S/Inputs/rdar51725203.swift

func test(sm: StructModel, cm: ClassModel) {
  _ = sm.$foo
  _ = cm.$foo

  _ = sm._bar // expected-error{{'_bar' is inaccessible due to 'private' protection level}}
  _ = cm._bar // expected-error{{'_bar' is inaccessible due to 'private' protection level}}
}
