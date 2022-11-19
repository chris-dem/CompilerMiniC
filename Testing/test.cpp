#include <algorithm>
#include <deque>
#include <iostream>
#include <list>
#include <memory>
#include <numeric>
#include <optional>
#include <set>
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

int foo(int x) {
    if (x)
        return 3;
    else
        return 4;
    return 5;
    a = 4;
}

// std::optional<int> sum(std::optional<int> a, std::optional<int> b) {
//     return a.value_or(b);
// }

// std::optional<int> map(std::optional<char> b) {
//     if (!b)
//         return b;
//     return std::optional(static_cast<int>(b.value()));
// }

int main() {
    int n;
    // auto res       = std::transform_reduce(st.cbegin(), st.cend(), sum,
    // map);
    std::set<int> st = {1, 2, 3, 5};
    st.insert(6);
    if (st.find(6) != st.end()) {
        std::cout << "Found" << std::endl;
    } else {
        std::cout << "End" << std::endl;
    }
    return 0;
}