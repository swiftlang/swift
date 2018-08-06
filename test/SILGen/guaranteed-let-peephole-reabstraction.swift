// RUN: %target-swift-emit-silgen -enable-sil-ownership -verify %s 
class BlockBox<T> {
  let block: (T) -> Void = { _ in }

  var computedBlock: (T) -> Void { return { _ in } }
}

struct BlockStruct<T> {
  let block: (T) -> Void = { _ in }
  var computedBlock: (T) -> Void { return { _ in } }
}

func escapingCompletion(completion: @escaping (String) -> Void) {}

func foo(box: BlockBox<String>) {
  escapingCompletion(completion: box.block)
  escapingCompletion(completion: box.computedBlock)
}
func foo(struc: BlockStruct<String>) {
  escapingCompletion(completion: struc.block)
  escapingCompletion(completion: struc.computedBlock)
}
