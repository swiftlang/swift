// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -verify -swift-version 3 %s

// REQUIRES: objc_interop

import Foundation
import objc_generics

// Test type-erased bounds

func getContainerForPanda() -> AnimalContainer<Animal> {
  return Panda.getContainer()
}

// Note that in Swift 3, there's basically no way to call the
// Panda.getFungibleContainer() method, because it is imported
// with a malformed type.

// expected-error@+1 {{type 'Animal' does not conform to protocol 'Fungible'}}
func getContainerForFungiblePanda() -> FungibleAnimalContainer<Animal> {
  return Panda.getFungibleContainer()
}


func getContainerForFungiblePanda() -> FungibleAnimalContainer<Animal & Fungible> {
  // expected-error@+1 {{cannot convert return expression of type 'FungibleAnimalContainer<Animal>!' to return type 'FungibleAnimalContainer<Animal & Fungible>'}}
  return Panda.getFungibleContainer()
}
