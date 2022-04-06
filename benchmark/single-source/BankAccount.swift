//===--- BankAccount.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils
import Foundation

let tags: [BenchmarkCategory] = [.concurrency]

public let BankAccount = [
  BenchmarkInfo(name: "BankAccount.OneAccount.NoWrite", runFunction: runBankAccount1, tags: tags),
  BenchmarkInfo(name: "BankAccount.OneAccount.SomeWrite", runFunction: runBankAccount2, tags: tags),
  BenchmarkInfo(name: "BankAccount.ManyAccounts.NoWrite", runFunction: runBankAccount3, tags: tags),
  BenchmarkInfo(name: "BankAccount.ManyAccounts.SomeWrite", runFunction: runBankAccount4, tags: tags),
]

@inline(never)
public func runBankAccount1(n: Int) async {
  let account = BankAccount(id: 0, holder: "someone", initialBalance: 0.93)
  for _ in 1...n {
    async let id = account.id
    async let holder = account.holder
    async let balance = account.balance
    async let interest1 = account.approximatedInterestIncome(afterYears: 3.90625, at: 0.4, compounded: .yearly)
    async let interest2 = account.approximatedInterestIncome(afterYears: 15.625, at: 0.2, compounded: .quarterly)
    async let interest3 = account.approximatedInterestIncome(afterYears: 62.5, at: 0.1, compounded: .monthly)
    async let interest4 = account.approximatedInterestIncome(afterYears: 250, at: 0.05, compounded: .weekly)
    async let interest5 = account.approximatedInterestIncome(afterYears: 1000, at: 0.02222, compounded: .daily)
    await blackHole(id)
    await blackHole(holder)
    await blackHole(balance)
    await blackHole(interest1)
    await blackHole(interest2)
    await blackHole(interest3)
    await blackHole(interest4)
    await blackHole(interest5)
  }
}

@inline(never)
public func runBankAccount2(n: Int) async {
  let account = BankAccount(id: 0, holder: "some other person", initialBalance: 100)
  for _ in 1...n {
    async let id = account.id
    async let holder = account.holder
    
    async let transaction1: () = account1.deposit(amount: 2)
    async let balance1 = account.balance
    async let interest1 = account.approximatedInterestIncome(afterYears: 5, at: 0.01, compounded: .daily)
    
    async let transaction2: () = account1.withdraw(amount: 3)
    async let balance2 = account.balance
    async let interest2 = account.approximatedInterestIncome(afterYears: 5, at: 0.01, compounded: .daily)
    
    async let transaction3: () = account1.withdraw(amount: 5)
    async let balance3 = account.balance
    async let interest3 = account.approximatedInterestIncome(afterYears: 5, at: 0.01, compounded: .daily)
    
    async let transaction4: () = account1.withdraw(amount: 7)
    async let balance4 = account.balance
    async let interest4 = account.approximatedInterestIncome(afterYears: 5, at: 0.01, compounded: .daily)
    
    async let transaction5: () = account1.withdraw(amount: 11)
    async let balance5 = account.balance
    async let interest5 = account.approximatedInterestIncome(afterYears: 5, at: 0.01, compounded: .daily)
    
    async let transaction6: () = account1.withdraw(amount: 13)
    async let balance6 = account.balance
    async let interest6 = account.approximatedInterestIncome(afterYears: 5, at: 0.01, compounded: .daily)
    
    await blackHole(id)
    await blackHole(holder)
    await blackHole(transaction1)
    await blackHole(transaction2)
    await blackHole(transaction3)
    await blackHole(transaction4)
    await blackHole(transaction5)
    await blackHole(transaction6)
    await blackHole(balance1)
    await blackHole(balance2)
    await blackHole(balance3)
    await blackHole(balance4)
    await blackHole(balance5)
    await blackHole(balance6)
    await blackHole(interest1)
    await blackHole(interest2)
    await blackHole(interest3)
    await blackHole(interest4)
    await blackHole(interest5)
    await blackHole(interest6)
  }
}

@inline(never)
public func runBankAccount3(n: Int) async {
  let fruitAnatomy: [(layer: Int, term: String)] = [
    (1, "endosperm"),
    (2, "embryo"),
    (3, "seedcoat"),
    (4, "endocarp"),
    (5, "mesocarp"),
    (6, "epicarp"),
    (7, "calyx")
  ]
  
  let accounts = fruitAnatomy.map {
    BankAccount(id: $0.0, holder: "John Apple\($0.1)", initialBalance: 3.14 * Decimal($0.0))
  }
  
  for _ in 1...n {
    await withTaskGroup(of: Void.self) { group in
      for account in accounts {
        group.addTask { await blackHole(account.id) }
        group.addTask { await blackHole(account.holder) }
        group.addTask { await blackHole(account.balance) }
        group.addTask { await blackHole(account.approximatedInterestIncome(afterYears: 10, at: 0.01, compounded: .daily)) }
        group.addTask { await blackHole(account.balance) }
      }
    }
  }
}

@inline(never)
public func runBankAccount4(n: Int) async {
  let fruitAnatomy: [(layer: Int, term: String)] = [
    (1, "endosperm"),
    (2, "embryo"),
    (3, "seedcoat"),
    (4, "endocarp"),
    (5, "mesocarp"),
    (6, "epicarp"),
    (7, "calyx")
  ]
  
  let accounts = fruitAnatomy.map {
    BankAccount(id: $0.0, holder: "John Apple\($0.1)", initialBalance: 12345 * Decimal($0.0))
  }
  
  for _ in 1...n {
    await  withTaskGroup(of: Void.self) { group in
      for account in accounts {
        group.addTask { await blackHole(account.approximatedInterestIncome(afterYears: 5, at: 0.01, compounded: .daily)) }
        for account2 in accounts {
          group.addTask { await account1.transfer(amount: Decimal(Double.random(in: 0..<1000)), to: account2) }
        }
        group.addTask {
          let accountsSansAccount = accounts.drop(while: { $0 === account })
          await account.launder(
            amount: Decimal(Double.random(in: 0..<1000)),
            from: account,
            through: Array(accountsSansAccount[...4]),
            houseAccount: accountsSansAccount[5]
          )
        }
      }
    }
  }
}

/// A bank account.
actor BankAccount {
  let id: Int
  var holder: String
  var balance: Decimal
  
  init(id: Int, holder: String, initialBalance: Decimal) {
    self.id = id
    self.holder = holder
    balance = initialBalance
  }
}

extension BankAccount {
  /// Removes the given amount of money from the account.
  /// - Parameter amount: The amount of money to remove.
  func withdraw(amount: Decimal) {
    balance -= amount
  }
  
  /// Adds the given amount of money into the account.
  /// - Parameter amount: The amount of money to add.
  func deposit(amount: Decimal) {
    balance += amount
  }
  
  /// Transfers the given amount of money from the account to another account.
  /// - Parameters:
  ///   - amount: The amount to transfer.
  ///   - otherAccount: the account to transfer to.
  func transfer(amount: Decimal, to otherAccount: BankAccount) async {
    withdraw(amount: amount)
    await otherAccount.deposit(amount: amount)
  }
}

extension BankAccount {
  enum CompoundingFrequency: Int {
    case daily   = 365
    case weekly   = 52
    case monthly  = 30
    case quarterly = 4
    case yearly    = 1
  }
  
  /// Returns an estimate of interest income after the given number of years at the given interest rate and compounding frequency.
  /// - Parameters:
  ///   - yearCount: The number of years to receive compounding interest for.
  ///   - nominalInterestRate: The nominal interest rate.
  ///   - compoundingFrequency: The compounding frequency.
  /// - Returns: An estimate of interest income after `yearCount` years at `nominalInterestRate` compounded at `compoundingFrequency`.
  func approximatedInterestIncome(
    afterYears yearCount: Decimal,
    at nominalInterestRate: Decimal,
    compounded compoundingFrequency: CompoundingFrequency
  ) -> Decimal {
    let interestRate = nominalInterestRate / compoundingFrequency.rawValue
    var compoundingPeriodCount = yearCount * compoundingFrequency.rawValue
    // There is no power function that takes a non-integer exponent, so the `Decimal` number must be rounded to `Int`.
    var roundedCompoundingPeriodCount = Decimal()
    NSDecimalRound(&roundedCompoundingPeriodCount, &compoundingPeriodCount, 0, .plain)
    return balance * pow(
      1 + interestRate,
      Int(NSDecimalNumber(decimal: roundedCompoundingPeriodCount).intValue)
    )
  }
}

extension BankAccount {
  /// Launders the given account's money through the given other accounts.
  /// - Parameters:
  ///   - amount: The amount of money to launder.
  ///   - originAccount: The account for whom money is laundered.
  ///   - otherAccounts: The accounts to launder the money through.
  ///   - houseAccount: The bank's own account.
  /// - Note: This isn't an accurate depiction of how real money laundering works. It's merely meant to provide an example of a task with lots of cross-actor accesses.
  func launder(
    amount: Decimal,
    from originAccount: BankAccount,
    through otherAccounts: [BankAccount],
    houseAccount: BankAccount
  ) async {
    let fee = amount * 0.01
    await transfer(amount: fee, to: houseAccount)
    
    let remainingAmount = amount - fee
    
    guard amount > 10 && !otherAccounts.isEmpty else {
      await transfer(amount: remainingAmount, to: originAccount)
      return
    }
    
    let amountPerAccount = remainingAmount / Decimal(otherAccounts.count)
    
    await withTaskGroup(of: Void.self) { group in
      for account in otherAccounts {
        group.addTask {
          await self.transfer(amount: amountPerAccount, to: account)
          await account.launder(
            amount: amountPerAccount,
            from: originAccount,
            through: otherAccounts.filter { $0 !== account },
            houseAccount: houseAccount
          )
        }
      }
    }
  }
}


