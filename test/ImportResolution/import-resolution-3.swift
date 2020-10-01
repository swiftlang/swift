// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/asdf.swift
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/sdf.swift -I %t
// RUN: %target-swift-frontend -typecheck %s -I %t -verify

import asdf
import sdf
import struct sdf.S

var uS: S = sdf.S(x: 123)