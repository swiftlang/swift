//===--- Action.h - Abstract compilation steps ------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DRIVER_ACTION_H
#define SWIFT_DRIVER_ACTION_H

#include "swift/Basic/LLVM.h"
#include "swift/Driver/Types.h"
#include "swift/Driver/Util.h"
#include "llvm/ADT/ArrayRef.h"

namespace llvm {
namespace opt {
  class Arg;
}
}

namespace swift {
namespace driver {
  class Action;

class Action {
public:
  typedef ActionList::size_type size_type;
  typedef ActionList::iterator iterator;
  typedef ActionList::const_iterator const_iterator;

  enum ActionClass {
    Input = 0,
    CompileJob,
    LinkJob,

    JobFirst=CompileJob,
    JobLast=LinkJob
  };

  static const char *getClassName(ActionClass AC);

private:
  ActionClass Kind;
  types::ID Type;

  ActionList Inputs;

  unsigned OwnsInputs : 1;

protected:
  Action(ActionClass Kind, types::ID Type)
    : Kind(Kind), Type(Type), OwnsInputs(true) {}
  Action(ActionClass Kind, ArrayRef<Action *> Inputs, types::ID Type)
    : Kind(Kind), Type(Type), Inputs(Inputs.begin(), Inputs.end()),
      OwnsInputs(true) {}

public:
  virtual ~Action();

  const char *getClassName() const { return Action::getClassName(getKind()); }

  bool getOwnsInputs() const { return OwnsInputs; }
  void setOwnsInputs(bool Value) { OwnsInputs = Value; }

  ActionClass getKind() const { return Kind; }
  types::ID getType() const { return Type; }

  ArrayRef<Action *> getInputs() const { return Inputs; }

  size_type size() const { return Inputs.size(); }

  iterator begin() { return Inputs.begin(); }
  iterator end() { return Inputs.end(); }
  const_iterator begin() const { return Inputs.begin(); }
  const_iterator end() const { return Inputs.end(); }
};

class InputAction : public Action {
  virtual void anchor();
  const llvm::opt::Arg &Input;

public:
  InputAction(const llvm::opt::Arg &Input, types::ID Type)
      : Action(Action::Input, Type), Input(Input) {}
  const llvm::opt::Arg &getInputArg() const { return Input; }

  static bool classof(const Action *A) {
    return A->getKind() == Action::Input;
  }
};

class JobAction : public Action {
  virtual void anchor();
protected:
  JobAction(ActionClass Kind, ArrayRef<Action *> Inputs, types::ID Type)
      : Action(Kind, Inputs, Type) {}

public:
  static bool classof(const Action *A) {
    return (A->getKind() >= ActionClass::JobFirst &&
            A->getKind() <= ActionClass::JobLast);
  }
};

class CompileJobAction : public JobAction {
  virtual void anchor();
public:
  CompileJobAction(Action *Input, types::ID OutputType)
      : JobAction(Action::CompileJob, Input, OutputType) {}

  static bool classof(const Action *A) {
    return A->getKind() == Action::CompileJob;
  }
};

class LinkJobAction : public JobAction {
  virtual void anchor();
public:
  LinkJobAction(ArrayRef<Action *> Inputs)
      : JobAction(Action::LinkJob, Inputs, types::TY_Image) {}

  static bool classof(const Action *A) {
    return A->getKind() == Action::LinkJob;
  }
};

} // end namespace driver
} // end namespace swift

#endif
