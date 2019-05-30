// RUN: %target-run-simple-swift | %FileCheck %s

// REQUIRES: executable_test

public class BaseView { }
public class GenericView<T>: BaseView { }
public class FinalView: GenericView<ContentForTheView> { }


public class ContentForTheView { }
extension ContentForTheView: InfoNeededByControllers { }

public protocol ConditionallyConformed { }
public protocol InfoNeededByControllers { }

extension GenericView: ConditionallyConformed where T: InfoNeededByControllers { }

open class BaseGenericController<T> where T: BaseView & ConditionallyConformed { }


open class FinalController: BaseGenericController<FinalView> { public override init() { } }

// CHECK: FinalController
print(FinalController())
