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
    BackendJob,
    MergeModuleJob,
    AutolinkExtractJob,
    REPLJob,
    LinkJob,
    GenerateDSYMJob,

    JobFirst=CompileJob,
    JobLast=GenerateDSYMJob
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
  void addInput(Action *Input) { Inputs.push_back(Input); }

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
  
  // Returns the index of the Input action's output file which is used as
  // (single) input to this action. Most actions produce only a single output
  // file, so we return 0 by default.
  virtual int getInputIndex() const { return 0; }
};

class CompileJobAction : public JobAction {
public:
  enum class BuildState {
    UpToDate,
    NeedsCascadingBuild,
    NeedsNonCascadingBuild
  };

private:
  virtual void anchor();
  BuildState PreviousBuildState;

public:
  CompileJobAction(types::ID OutputType)
      : JobAction(Action::CompileJob, llvm::None, OutputType),
        PreviousBuildState(BuildState::UpToDate) {}

  CompileJobAction(Action *Input, types::ID OutputType,
                   BuildState previousState)
      : JobAction(Action::CompileJob, Input, OutputType),
        PreviousBuildState(previousState) {}

  BuildState getPreviousBuildState() const {
    return PreviousBuildState;
  }

  static bool classof(const Action *A) {
    return A->getKind() == Action::CompileJob;
  }
};

class BackendJobAction : public JobAction {
private:
  virtual void anchor();
  
  // In case of multi-threaded compilation, the compile-action produces multiple
  // output bitcode-files. For each bitcode-file a BackendJobAction is created.
  // This index specifies which of the files to select for the input.
  int InputIndex;
public:
  BackendJobAction(Action *Input, types::ID OutputType, int InputIndex)
      : JobAction(Action::BackendJob, Input, OutputType),
        InputIndex(InputIndex) {}
  static bool classof(const Action *A) {
    return A->getKind() == Action::BackendJob;
  }
  
  virtual int getInputIndex() const { return InputIndex; }
};

class REPLJobAction : public JobAction {
public:
  enum class Mode {
    Integrated,
    PreferLLDB,
    RequireLLDB
  };
private:
  virtual void anchor();
  Mode RequestedMode;
public:
  REPLJobAction(Mode mode)
    : JobAction(Action::REPLJob, llvm::None, types::TY_Nothing),
      RequestedMode(mode) {}

  Mode getRequestedMode() const { return RequestedMode; }

  static bool classof(const Action *A) {
    return A->getKind() == Action::REPLJob;
  }
};

class MergeModuleJobAction : public JobAction {
  virtual void anchor();
public:
  MergeModuleJobAction(ArrayRef<Action *> Inputs)
      : JobAction(Action::MergeModuleJob, Inputs, types::TY_SwiftModuleFile) {}

  static bool classof(const Action *A) {
    return A->getKind() == Action::MergeModuleJob;
  }
};

class AutolinkExtractJobAction : public JobAction {
  virtual void anchor();
public:
  AutolinkExtractJobAction(ArrayRef<Action *> Inputs)
      : JobAction(Action::AutolinkExtractJob, Inputs, types::TY_AutolinkFile) {}

  static bool classof(const Action *A) {
    return A->getKind() == Action::AutolinkExtractJob;
  }
};

class GenerateDSYMJobAction : public JobAction {
  virtual void anchor();
public:
  explicit GenerateDSYMJobAction(Action *Input)
    : JobAction(Action::GenerateDSYMJob, Input, types::TY_dSYM) {}

  static bool classof(const Action *A) {
    return A->getKind() == Action::GenerateDSYMJob;
  }
};

class LinkJobAction : public JobAction {
  virtual void anchor();
  LinkKind Kind;

public:
  LinkJobAction(ArrayRef<Action *> Inputs, LinkKind K)
      : JobAction(Action::LinkJob, Inputs, types::TY_Image), Kind(K) {
    assert(Kind != LinkKind::None);
  }

  LinkKind getKind() const { return Kind; }

  static bool classof(const Action *A) {
    return A->getKind() == Action::LinkJob;
  }
};

} // end namespace driver
} // end namespace swift

#endif
