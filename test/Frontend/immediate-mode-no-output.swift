// REQUIRES: swift_interpreter
// RUN: %empty-directory(%t)
// RUN: cd %t && %target-swift-frontend -interpret %S/../Inputs/empty.swift
// RUN: not ls %t/*
