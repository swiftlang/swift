// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-swift-frontend -primary-file %t/main.swift %S/Inputs/sr8493-helper.swift -emit-ir -o /dev/null

StateAnimatorGroup<String>().set("")