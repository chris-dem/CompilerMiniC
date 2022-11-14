#include <deque>
#include <iostream>
#include <list>
#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <vector>

class A {
  public:
    virtual void foo() {
        std::cout << "Hi I am A" << std::endl;
    }
};

class B : public A {
  public:
    virtual void foo() override {
        std::cout << "Hi I am B" << std::endl;
    }
};
class C : public A {
  public:
    virtual void foo() override {
        std::cout << "Hi I am C" << std::endl;
    }
};

int main() {
    std::unique_ptr<A> f = std::make_unique<C>();
    if (!dynamic_cast<B*>(f.get())) {
        std::cout << "At B" << std::endl;
        f->foo(); // Can still use
    } else {
        std::cout << "At C" << std::endl;
        f->foo();
    }
    return 0;
}