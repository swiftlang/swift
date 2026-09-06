// Executable end-to-end test: this C++ file defines the key functions of the
// classes in virtual.h, so this translation unit emits their vtables, whose
// slots name the virtual methods implemented in Swift via
// `@cxx @implementation`.

// RUN: %empty-directory(%t)
// RUN: %target-interop-build-clangxx \
// RUN:   -c %s \
// RUN:   -I %S/Inputs \
// RUN:   -o %t/virtual-execution-main.o
// RUN: %target-interop-build-swift \
// RUN:   -enable-experimental-feature CxxImplementation \
// RUN:   -Xfrontend -disable-availability-checking \
// RUN:   -module-name VirtualExecutionMain \
// RUN:   -parse-as-library \
// RUN:   -I %S/Inputs \
// RUN:   -Xlinker %t/virtual-execution-main.o \
// RUN:   %S/Inputs/virtual-execution.swift \
// RUN:   -o %t/virtual-execution
// RUN: %target-codesign %t/virtual-execution
// RUN: %target-run %t/virtual-execution | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_CxxImplementation

#include <stdio.h>

#include "virtual.h"

// The key functions: defining them here makes this the translation unit that
// emits the vtables of Shape, SimpleBase, SimpleDerived, and Engine.
int Shape::keyFunction() const { return sides; }
void SimpleBase::sbAnchor() {}
void SimpleDerived::sdAnchor() {}
void Engine::keyAnchor() {}

// Retains minus releases.
static int liveEngines = 0;

void retainEngine(Engine *) { ++liveEngines; }
void releaseEngine(Engine *) { --liveEngines; }

// Dispatch virtually through a pointer whose dynamic type the compiler cannot
// see through.
__attribute__((noinline)) static int callArea(const Shape *shape) {
  return shape->area();
}
__attribute__((noinline)) static void callScale(Shape *shape, int factor) {
  shape->scale(factor);
}
__attribute__((noinline)) static int callSimple(const SimpleBase *simple) {
  return simple->simple();
}
__attribute__((noinline)) static int callStatus(const Engine *engine) {
  return engine->status();
}
__attribute__((noinline)) static void callBoost(Engine *engine, int amount) {
  engine->boost(amount);
}

// A C++ override of a Swift-implemented method: Pentagon's vtable overrides
// area() in C++ and inherits the slot for the Swift-implemented scale().
struct Pentagon : Shape {
  int area() const override { return 5 * sides; }
};

int main() {
  Shape shape;
  shape.sides = 4;

  int area = callArea(&shape);
  printf("area=%d\n", area);
  // CHECK: area=16

  callScale(&shape, 3);
  printf("scaled=%d\n", shape.sides);
  // CHECK: scaled=12

  printf("keyFunction=%d\n", shape.keyFunction());
  // CHECK: keyFunction=12

  // The C++ override wins on a Pentagon; the inherited scale() slot still
  // dispatches to the Swift implementation.
  Pentagon pentagon;
  pentagon.sides = 4;

  int pentagonArea = callArea(&pentagon);
  printf("pentagonArea=%d\n", pentagonArea);
  // CHECK: pentagonArea=20

  callScale(&pentagon, 2);
  printf("pentagonScaled=%d\n", pentagon.sides);
  // CHECK: pentagonScaled=8

  // Both the base method and its accepted Swift-implemented override dispatch
  // through the same slot to the dynamic type's implementation.
  SimpleBase base;
  base.stored = 5;

  int baseSimple = callSimple(&base);
  printf("baseSimple=%d\n", baseSimple);
  // CHECK: baseSimple=5

  SimpleDerived derived;
  derived.stored = 7;

  int derivedSimple = callSimple(&derived);
  printf("derivedSimple=%d\n", derivedSimple);
  // CHECK: derivedSimple=14

  Engine engine;
  engine.rpm = 1000;

  int status = callStatus(&engine);
  printf("status=%d live=%d\n", status, liveEngines);
  // CHECK: status=1000 live=0

  callBoost(&engine, 500);
  printf("boosted=%d live=%d\n", engine.rpm, liveEngines);
  // CHECK: boosted=1500 live=0

  return 0;
}
