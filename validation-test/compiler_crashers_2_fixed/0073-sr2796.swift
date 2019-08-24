// RUN: %target-swift-frontend %s -emit-ir

protocol ViewModel {}

protocol ViewModelCell {}

protocol CellAwareViewModel : ViewModel {
    associatedtype CellType: ViewModelCell
}

protocol ConfigurableViewModelCell : ViewModelCell {
    associatedtype DataType: CellAwareViewModel
}

func useType<T: ViewModelCell>(cellType: T.Type) {
}

class ConfigurableViewModelCellProvider<V, C> where V: CellAwareViewModel,
                                                    C: ConfigurableViewModelCell,
                                                    C.DataType == V,
                                                    V.CellType == C {
  static func crasher() {
    // IRGen for the metatype instruction 
    useType(cellType: C.self)
  }
}
