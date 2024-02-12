#pragma once

struct InstanceBalanceCounter {
    static inline int &theCounterValue() {
        static int counterValue = 0;
        return counterValue;
    }
    static inline int getCounterValue() {
        return theCounterValue();
    }
    
    __attribute__((optnone))
    InstanceBalanceCounter() {
        ++theCounterValue();
    }
    __attribute__((optnone))
    InstanceBalanceCounter(const InstanceBalanceCounter &) {
        ++theCounterValue();
    }
    __attribute__((optnone))
    InstanceBalanceCounter(InstanceBalanceCounter &&) {
        ++theCounterValue();
    }
    __attribute__((optnone))
    ~InstanceBalanceCounter() {
        --theCounterValue();
    }
};

__attribute__((optnone))
inline void someFunc() {}

struct NonTrivialCopyAssign {
    __attribute__((optnone))
    NonTrivialCopyAssign(): copyAssignCounter(0) {}
    __attribute__((optnone))
    ~NonTrivialCopyAssign() {
        someFunc();
    }

    __attribute__((optnone))
    NonTrivialCopyAssign &operator =(const NonTrivialCopyAssign &) {
        ++copyAssignCounter;
        return *this;
    }

    int copyAssignCounter;
    InstanceBalanceCounter instanceBalancer;
};

struct NonTrivialMoveAssign {
    __attribute__((optnone))
    NonTrivialMoveAssign(): moveAssignCounter(0) {}
    __attribute__((optnone))
    NonTrivialMoveAssign(const NonTrivialMoveAssign &) = default;
    __attribute__((optnone))
    ~NonTrivialMoveAssign() {
        someFunc();
    }

    __attribute__((optnone))
    NonTrivialMoveAssign &operator =(NonTrivialMoveAssign &&) {
        ++moveAssignCounter;
        return *this;
    }

    int moveAssignCounter;
    InstanceBalanceCounter instanceBalancer;
};

struct NonTrivialCopyAndCopyMoveAssign {
    __attribute__((optnone))
    NonTrivialCopyAndCopyMoveAssign(): assignCounter(0) {}
    __attribute__((optnone))
    NonTrivialCopyAndCopyMoveAssign(const NonTrivialCopyAndCopyMoveAssign &other) : assignCounter(other.assignCounter), instanceBalancer(other.instanceBalancer) {
        someFunc();
    }
    __attribute__((optnone))
    NonTrivialCopyAndCopyMoveAssign( NonTrivialCopyAndCopyMoveAssign &&other) : assignCounter(other.assignCounter), instanceBalancer(other.instanceBalancer) {
        someFunc();
    }
    __attribute__((optnone))
    ~NonTrivialCopyAndCopyMoveAssign() {
        someFunc();
    }

    __attribute__((optnone))
    NonTrivialCopyAndCopyMoveAssign &operator =(const NonTrivialCopyAndCopyMoveAssign &) {
        ++assignCounter;
        return *this;
    }
    __attribute__((optnone))
    NonTrivialCopyAndCopyMoveAssign &operator =(NonTrivialCopyAndCopyMoveAssign &&) {
        ++assignCounter;
        return *this;
    }

    int assignCounter;
    InstanceBalanceCounter instanceBalancer;
};
